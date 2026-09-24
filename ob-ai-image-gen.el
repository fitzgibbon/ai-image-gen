;;; ob-ai-image-gen.el --- Org Babel support for ai-image-gen -*- lexical-binding: t; -*-

;; Author: Niall FitzGibbon

;; This file is part of ai-image-gen.

;;; Commentary:

;; The `ai-image-gen' Babel language.  Org loads this file by name when
;; the language is enabled:
;;
;;   (add-to-list 'org-babel-load-languages '(ai-image-gen . t))
;;
;; The block body is the prompt; the result is a link to the image.
;;
;;   #+begin_src ai-image-gen :size 768x512 :seed 7
;;   A platypus in a hard hat, flat vector style
;;   #+end_src
;;
;; Header arguments: :provider (a registered name), :size, :seed, :steps,
;; :guidance and the usual :file.  Without :file the image goes to
;; `ai-image-gen-directory'.
;;
;; :image turns the block into an edit of one or more source images, each a
;; file name or the name of another block (whose result is used) or of a
;; named link:
;;
;;   #+begin_src ai-image-gen :image mascot :seed 7
;;   Same platypus, now holding a wrench
;;   #+end_src
;;
;; Several sources are space-separated or a quoted list.  :mask names a PNG
;; whose transparent areas mark what to change, for providers that take one.
;;
;; Run from a command, a block returns at once with its link and the image
;; fills it in when it arrives; an edit waits for sources still generating.
;; Export, :async no and a nil `ob-ai-image-gen-async' make blocks wait.

;;; Code:

(require 'ob)
(require 'ol)
(require 'ai-image-gen)

(defvar org-babel-default-header-args:ai-image-gen
  '((:results . "file graphics")
    (:exports . "results"))
  "Default header arguments for ai-image-gen blocks.")

(defconst org-babel-header-args:ai-image-gen
  '((provider . :any)
    (size . :any)
    (seed . :any)
    (steps . :any)
    (guidance . :any)
    (image . :any)
    (mask . :any)
    (async . ((yes no))))
  "Header arguments specific to ai-image-gen blocks.")

(defcustom ob-ai-image-gen-async t
  "Whether blocks run from a command generate in the background.
The block's link is written at once and the image fills it in when it
arrives.  Blocks run during export always wait, since the exported
document needs the image.  A block's :async header argument overrides
this."
  :type 'boolean
  :group 'ai-image-gen)

(defvar ob-ai-image-gen--pending (make-hash-table :test 'equal)
  "Files being generated in the background, mapped to their waiters.
A waiter is called with nil once the file is written, or with an
error message if generating it failed.")

(defun ob-ai-image-gen--number (params key)
  "Return header argument KEY of PARAMS as a number, or nil."
  (let ((value (cdr (assq key params))))
    (cond
     ((null value) nil)
     ((numberp value) value)
     ((and (stringp value) (string-match-p "\\`[0-9.]+\\'" value))
      (string-to-number value))
     (t (user-error "Header argument %s must be a number, got %S" key value)))))

(defun ob-ai-image-gen--string (params key)
  "Return header argument KEY of PARAMS as a string, or nil."
  (when-let* ((value (cdr (assq key params))))
    (format "%s" value)))

(defun ob-ai-image-gen--link-file-at-point ()
  "Return the file of the link starting at point, skipping blanks, or nil."
  (skip-chars-forward " \t")
  (when (looking-at org-link-bracket-re)
    (org-babel-read-link)))

(defun ob-ai-image-gen--named-image (name)
  "Return the image file behind the Org element named NAME, or nil.
A named source block yields its result, and is executed first when it
has none.  Any other named element yields the link that starts it."
  (save-excursion
    (if-let* ((block (org-babel-find-named-block name)))
        (progn
          (goto-char block)
          (let ((result (or (org-babel-where-is-src-block-result)
                            (progn (org-babel-execute-src-block)
                                   (org-babel-where-is-src-block-result)))))
            (when result
              (goto-char result)
              (forward-line)
              (ob-ai-image-gen--link-file-at-point))))
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward
               (format "^[ \t]*#\\+name:[ \t]*%s[ \t]*$" (regexp-quote name)) nil t)
          (forward-line)
          (ob-ai-image-gen--link-file-at-point))))))

(defun ob-ai-image-gen--references (value)
  "Split header argument VALUE into image references.
VALUE is a string of space-separated references or a list of them."
  (cond
   ((null value) nil)
   ((listp value) (mapcar (lambda (item) (format "%s" item)) value))
   (t (split-string-and-unquote (format "%s" value)))))

(defun ob-ai-image-gen--resolve-image (reference)
  "Return the image file for REFERENCE, a block name or a file name."
  (or (ob-ai-image-gen--named-image reference)
      (let ((file (expand-file-name reference)))
        (and (file-readable-p file) file))
      (user-error "No image file or named block %s" reference)))

(defun ob-ai-image-gen-request (body params)
  "Return the request for block BODY with PARAMS.
With an :image header argument it is an `ai-image-gen-edit-request'."
  (let ((prompt (string-trim (org-babel-expand-body:generic body params)))
        (images (mapcar #'ob-ai-image-gen--resolve-image
                        (ob-ai-image-gen--references (cdr (assq :image params)))))
        (mask (when-let* ((mask (ob-ai-image-gen--string params :mask)))
                (ob-ai-image-gen--resolve-image mask))))
    (when (string-empty-p prompt)
      (user-error "Empty image prompt"))
    (when (and mask (not images))
      (user-error ":mask needs an :image to apply to"))
    (apply (if images #'ai-image-gen-edit-request-create #'ai-image-gen-request-create)
           :prompt prompt
           :size (ob-ai-image-gen--string params :size)
           :seed (ob-ai-image-gen--number params :seed)
           :steps (ob-ai-image-gen--number params :steps)
           :guidance (ob-ai-image-gen--number params :guidance)
           (when images (list :images images :mask mask)))))

(defun ob-ai-image-gen--async-p (params)
  "Return non-nil when the block with PARAMS should run in the background."
  (and (not (bound-and-true-p org-export-current-backend))
       (pcase (cdr (assq :async params))
         ('nil (and ob-ai-image-gen-async (not noninteractive)))
         ((or "no" "nil" 'no) nil)
         (_ t))))

(defun ob-ai-image-gen--pending-p (file)
  "Return non-nil while FILE is being generated in the background."
  (not (eq (gethash file ob-ai-image-gen--pending 'absent) 'absent)))

(defun ob-ai-image-gen--when-ready (files callback)
  "Call CALLBACK once none of FILES is being generated.
CALLBACK receives nil, or the error message of a source that failed."
  (let* ((pending (seq-filter #'ob-ai-image-gen--pending-p files))
         (remaining (length pending))
         (failure nil))
    (if (zerop remaining)
        (funcall callback nil)
      (dolist (file pending)
        (push (lambda (message)
                (setq failure (or failure message))
                (when (zerop (cl-decf remaining))
                  (funcall callback failure)))
              (gethash file ob-ai-image-gen--pending))))))

(defun ob-ai-image-gen--wait-for (files)
  "Wait until none of FILES is being generated, then check they exist."
  (with-timeout (ai-image-gen-timeout
                 (user-error "Timed out waiting for source images"))
    (while (seq-some #'ob-ai-image-gen--pending-p files)
      (accept-process-output nil 0.05)))
  (dolist (file files)
    (unless (and (file-readable-p file)
                 (> (file-attribute-size (file-attributes file)) 0))
      (user-error "Source image %s was not generated" file))))

(defun ob-ai-image-gen--update-links (file failure)
  "Show the links to FILE in the current buffer, or replace them with FAILURE."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-link-bracket-re nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0))
            (link (match-string-no-properties 1)))
        (when (and (string-prefix-p "file:" link)
                   (equal (expand-file-name (substring link 5)) file))
          (if failure
              (progn
                (delete-region beg end)
                (goto-char beg)
                (insert (format "ai-image-gen failed: %s" failure)))
            (cond
             ((fboundp 'org-link-preview-region)
              (org-link-preview-region nil t beg end))
             ((fboundp 'org-display-inline-images)
              (org-display-inline-images nil t beg end)))))))))

(defun ob-ai-image-gen--finish (buffer file failure reserved)
  "Record that FILE finished, with FAILURE or nil, and update BUFFER.
RESERVED means FILE is an empty placeholder to delete on failure."
  (let ((waiters (gethash file ob-ai-image-gen--pending)))
    (remhash file ob-ai-image-gen--pending)
    (when (and failure reserved)
      (ignore-errors (delete-file file)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (ob-ai-image-gen--update-links file failure)))
    (if failure
        (message "Image generation failed: %s" failure)
      (message "Generated %s" (abbreviate-file-name file)))
    (dolist (waiter (reverse waiters))
      (funcall waiter failure))))

(defun ob-ai-image-gen--start (request provider file sources reserved)
  "Generate REQUEST with PROVIDER into FILE in the background.
Wait first for SOURCES that are still being generated.  RESERVED
means FILE is a fresh name, held by an empty placeholder meanwhile."
  (let ((buffer (current-buffer)))
    (when reserved
      (write-region "" nil file nil 'silent))
    (puthash file nil ob-ai-image-gen--pending)
    ;; After Org has written the block's link, so it can be updated.
    (run-at-time
     0 nil
     (lambda ()
       (ob-ai-image-gen--when-ready
        sources
        (lambda (failure)
          (if failure
              (ob-ai-image-gen--finish buffer file (format "source image failed: %s" failure)
                                       reserved)
            (ai-image-gen-generate
             request :provider provider
             :then (lambda (images)
                     (ai-image-gen-image-save (car images) file)
                     (ob-ai-image-gen--finish buffer file nil reserved))
             :else (lambda (message)
                     (ob-ai-image-gen--finish buffer file message reserved))))))))
    (message "Generating %s in the background..." (file-name-nondirectory file))
    file))

(defun org-babel-execute:ai-image-gen (body params)
  "Generate the image described by BODY and return its file.
With an :image header argument, edit that image instead.  Run from a
command, the image is generated in the background (see
`ob-ai-image-gen-async').  PARAMS are the block's header arguments."
  (let* ((request (ob-ai-image-gen-request body params))
         (provider (ai-image-gen-get-provider (ob-ai-image-gen--string params :provider)))
         (target (when-let* ((file (cdr (assq :file params))))
                   (expand-file-name file)))
         (sources (when (ai-image-gen-edit-request-p request)
                    (append (ai-image-gen-edit-request-images request)
                            (ensure-list (ai-image-gen-edit-request-mask request))))))
    (if (ob-ai-image-gen--async-p params)
        (ob-ai-image-gen--start request provider
                                (or target (ai-image-gen-default-file request))
                                sources (not target))
      (ob-ai-image-gen--wait-for sources)
      (let ((image (car (ai-image-gen-generate-sync request :provider provider))))
        (ai-image-gen-image-save
         image (or target (ai-image-gen-default-file request image)))))))

(defun org-babel-prep-session:ai-image-gen (_session _params)
  "Signal that ai-image-gen blocks have no sessions."
  (user-error "ai-image-gen blocks do not support sessions"))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("ai-image-gen" . text)))

(provide 'ob-ai-image-gen)
;;; ob-ai-image-gen.el ends here
