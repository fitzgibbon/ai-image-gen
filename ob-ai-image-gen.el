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
    (mask . :any))
  "Header arguments specific to ai-image-gen blocks.")

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

(defun org-babel-execute:ai-image-gen (body params)
  "Generate the image described by BODY and return its file.
With an :image header argument, edit that image instead.
PARAMS are the block's header arguments."
  (let* ((request (ob-ai-image-gen-request body params))
         (image (car (ai-image-gen-generate-sync
                      request :provider (ob-ai-image-gen--string params :provider))))
         (file (cdr (assq :file params))))
    (ai-image-gen-image-save
     image (if file
               (expand-file-name file)
             (ai-image-gen-default-file request image)))))

(defun org-babel-prep-session:ai-image-gen (_session _params)
  "Signal that ai-image-gen blocks have no sessions."
  (user-error "ai-image-gen blocks do not support sessions"))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("ai-image-gen" . text)))

(provide 'ob-ai-image-gen)
;;; ob-ai-image-gen.el ends here
