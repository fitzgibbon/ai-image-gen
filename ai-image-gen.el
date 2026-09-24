;;; ai-image-gen.el --- Generate images through registered providers -*- lexical-binding: t; -*-

;; Author: Niall FitzGibbon
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (plz "0.9") (org "9.6"))
;; Keywords: multimedia, tools

;;; Commentary:

;; A small provider registry for text-to-image generation.  A provider is a
;; struct deriving from `ai-image-gen-provider' with a method on
;; `ai-image-gen-provider-generate'.  Two are built in:
;;
;; - `ai-image-gen-openai': the OpenAI images API (/v1/images/generations).
;; - `ai-image-gen-mlx': mlx-openai-server's variant of it, which also takes
;;   seed, steps and guidance and reports the seed of each image.
;;
;; An `ai-image-gen-edit-request' edits source images instead of
;; generating from nothing; the same entry points take both, and the
;; OpenAI-compatible providers send edits to /v1/images/edits.
;;
;; Providers return decoded image bytes; saving and inserting them is the
;; caller's business (`ai-image-gen-image-save', `ai-image-gen-insert',
;; `ai-image-gen-edit').
;;
;; The package also provides an Org Babel language, `ai-image-gen', whose
;; blocks are prompts and whose results are image links; enable it through
;; `org-babel-load-languages' (see ob-ai-image-gen.el).
;;
;;   (ai-image-gen-register-provider
;;    (ai-image-gen-mlx-create :name "flux2-klein"
;;                             :url "https://localhost:11490/v1"
;;                             :model "black-forest-labs/FLUX.2-klein-9B"))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mailcap)
(require 'plz)
(require 'subr-x)

(defgroup ai-image-gen nil
  "Generate images through registered providers."
  :group 'multimedia
  :prefix "ai-image-gen-")

(defcustom ai-image-gen-directory (locate-user-emacs-file "ai-image-gen/")
  "Directory generated images are saved to when no file is given."
  :type 'directory)

(defcustom ai-image-gen-default-provider nil
  "Name of the provider used when none is given.
When nil and exactly one provider is registered, that one is used."
  :type '(choice (const :tag "Only registered provider" nil) string))

(defcustom ai-image-gen-timeout 300
  "Seconds to wait for a provider before giving up.
Local servers that load models on demand can take a while on the
first request."
  :type 'natnum)

(define-error 'ai-image-gen-error "Image generation failed")

;;;; Data

(cl-defstruct (ai-image-gen-request
               (:constructor ai-image-gen-request-create)
               (:copier nil))
  "What to generate.  Providers ignore fields they cannot express."
  (prompt nil :type string :documentation "Text prompt.")
  (size nil :type (or null string) :documentation "\"WIDTHxHEIGHT\", or nil for the provider default.")
  (n 1 :type natnum :documentation "Number of images.")
  (seed nil :type (or null natnum) :documentation "Seed of the first image.")
  (steps nil :type (or null natnum) :documentation "Inference steps.")
  (guidance nil :type (or null number) :documentation "Guidance scale."))

(cl-defstruct (ai-image-gen-edit-request
               (:include ai-image-gen-request)
               (:constructor ai-image-gen-edit-request--create)
               (:copier nil))
  "An edit of one or more source images.
Build it with `ai-image-gen-edit-request-create', which checks the files."
  (images nil :type list :documentation "Absolute source image files, at least one.")
  (mask nil :type (or null string) :documentation "PNG whose transparent areas mark what to change."))

(cl-defun ai-image-gen-edit-request-create (&rest args &key images mask &allow-other-keys)
  "Return an `ai-image-gen-edit-request' of IMAGES with optional MASK.
IMAGES is a file name or a non-empty list of them.  The remaining
ARGS are the `ai-image-gen-request' fields."
  (let* ((images (mapcar #'expand-file-name (ensure-list images)))
         (mask (and mask (expand-file-name mask))))
    (unless images
      (user-error "An image edit needs at least one source image"))
    (dolist (file (if mask (cons mask images) images))
      (unless (file-readable-p file)
        (user-error "Cannot read image %s" file)))
    (apply #'ai-image-gen-edit-request--create
           :images images :mask mask
           (cl-loop for (key value) on args by #'cddr
                    unless (memq key '(:images :mask)) append (list key value)))))

(cl-defstruct (ai-image-gen-image
               (:constructor ai-image-gen-image-create)
               (:copier nil))
  "One generated image."
  (data nil :type string :documentation "Unibyte image bytes.")
  (mime-type nil :type string :documentation "MIME type sniffed from DATA.")
  (seed nil :type (or null integer) :documentation "Seed reported by the provider, if any."))

;;;; Provider protocol

(cl-defstruct (ai-image-gen-provider
               (:constructor nil)
               (:copier nil))
  "Base type of image providers."
  (name nil :type string :documentation "Registry key."))

(cl-defgeneric ai-image-gen-provider-generate (provider request then else)
  "Generate REQUEST with PROVIDER asynchronously.
Call THEN with a list of `ai-image-gen-image', or ELSE with an error
message string.  Return a process or nil; deleting the process
cancels the request.")

(cl-defstruct (ai-image-gen-openai
               (:include ai-image-gen-provider)
               (:constructor ai-image-gen-openai-create)
               (:copier nil))
  "The OpenAI images API, or a server speaking it."
  (url "https://api.openai.com/v1" :type string :documentation "Base URL including the version segment.")
  (key nil :type (or null string function) :documentation "API key, or a function returning one.")
  (model nil :type (or null string) :documentation "Model name sent with each request."))

(cl-defstruct (ai-image-gen-mlx
               (:include ai-image-gen-openai (url "https://localhost:11490/v1"))
               (:constructor ai-image-gen-mlx-create)
               (:copier nil))
  "mlx-openai-server, which extends the images API with sampling controls.")

;;;; Registry

(defvar ai-image-gen--providers nil
  "Alist of provider name to provider.")

(defun ai-image-gen-register-provider (provider)
  "Register PROVIDER under its name, replacing any provider of that name."
  (cl-check-type provider ai-image-gen-provider)
  (let ((name (ai-image-gen-provider-name provider)))
    (unless (and (stringp name) (not (string-empty-p name)))
      (error "Provider needs a non-empty name"))
    (setf (alist-get name ai-image-gen--providers nil nil #'equal) provider)
    provider))

(defun ai-image-gen-unregister-provider (name)
  "Remove the provider registered as NAME."
  (setf (alist-get name ai-image-gen--providers nil t #'equal) nil))

(defun ai-image-gen-provider-names ()
  "Return the names of registered providers."
  (mapcar #'car ai-image-gen--providers))

(defun ai-image-gen-get-provider (&optional name)
  "Return the provider registered as NAME, or the default one."
  (let ((name (or name ai-image-gen-default-provider)))
    (cond
     (name (or (alist-get name ai-image-gen--providers nil nil #'equal)
               (user-error "No image provider named %s" name)))
     ((null ai-image-gen--providers)
      (user-error "No image providers registered"))
     ((cdr ai-image-gen--providers)
      (user-error "Several image providers registered; set `ai-image-gen-default-provider'"))
     (t (cdar ai-image-gen--providers)))))

(defun ai-image-gen-read-provider (prompt)
  "Read a registered provider name with PROMPT."
  (completing-read prompt (ai-image-gen-provider-names) nil t nil nil
                   (or ai-image-gen-default-provider
                       (car (ai-image-gen-provider-names)))))

;;;; OpenAI-compatible implementation

(cl-defgeneric ai-image-gen-openai--body (provider request)
  "Return the JSON body alist for REQUEST on PROVIDER.")

(cl-defmethod ai-image-gen-openai--body ((provider ai-image-gen-openai) request)
  (let ((body `((prompt . ,(ai-image-gen-request-prompt request))
                (n . ,(ai-image-gen-request-n request)))))
    (when-let* ((model (ai-image-gen-openai-model provider)))
      (push `(model . ,model) body))
    (when-let* ((size (ai-image-gen-request-size request)))
      (push `(size . ,size) body))
    (nreverse body)))

(cl-defmethod ai-image-gen-openai--body ((_provider ai-image-gen-mlx) request)
  (append (cl-call-next-method)
          (delq nil
                (list (when-let* ((seed (ai-image-gen-request-seed request)))
                        `(seed . ,seed))
                      (when-let* ((steps (ai-image-gen-request-steps request)))
                        `(steps . ,steps))
                      (when-let* ((guidance (ai-image-gen-request-guidance request)))
                        `(guidance_scale . ,guidance))))))

(cl-defmethod ai-image-gen-openai--body ((_provider ai-image-gen-mlx)
                                         (request ai-image-gen-edit-request))
  ;; mlx-openai-server edits take neither a mask nor a batch size.
  (when (ai-image-gen-edit-request-mask request)
    (signal 'ai-image-gen-error '("mlx-openai-server edits do not take a mask")))
  (unless (eql (ai-image-gen-request-n request) 1)
    (signal 'ai-image-gen-error '("mlx-openai-server edits return one image per request")))
  (assq-delete-all 'n (cl-call-next-method)))

(defun ai-image-gen--resolve-key (key)
  "Return KEY, calling it first when it is a function."
  (if (functionp key) (funcall key) key))

(defun ai-image-gen--headers (provider content-type)
  "Return request headers for PROVIDER sending CONTENT-TYPE."
  (let ((key (ai-image-gen--resolve-key (ai-image-gen-openai-key provider))))
    `(("Content-Type" . ,content-type)
      ,@(when (and key (not (string-empty-p key)))
          `(("Authorization" . ,(concat "Bearer " key)))))))

(defun ai-image-gen--sniff-mime-type (data)
  "Return the MIME type of image bytes DATA."
  (cond
   ((string-prefix-p "\x89PNG\r\n\x1a\n" data) "image/png")
   ((string-prefix-p "\xff\xd8\xff" data) "image/jpeg")
   ((and (string-prefix-p "RIFF" data)
         (>= (length data) 12)
         (string= (substring data 8 12) "WEBP"))
    "image/webp")
   ((or (string-prefix-p "GIF87a" data) (string-prefix-p "GIF89a" data)) "image/gif")
   (t "application/octet-stream")))

(defun ai-image-gen--parse-images (json)
  "Return `ai-image-gen-image' objects from a parsed images API JSON reply."
  (let ((data (alist-get 'data json)))
    (unless (and data (seq-every-p (lambda (item) (alist-get 'b64_json item)) data))
      (signal 'ai-image-gen-error '("Provider returned no b64_json image data")))
    (mapcar (lambda (item)
              (let ((bytes (base64-decode-string (alist-get 'b64_json item))))
                (ai-image-gen-image-create
                 :data bytes
                 :mime-type (ai-image-gen--sniff-mime-type bytes)
                 :seed (alist-get 'seed item))))
            data)))

(defun ai-image-gen--error-message (body)
  "Extract a readable error from response BODY, a JSON string or nil."
  (let ((json (ignore-errors (json-parse-string (or body "") :object-type 'alist))))
    (or (and (consp json)
             (let ((detail (alist-get 'detail json))
                   (error (alist-get 'error json)))
               (cond
                ((stringp detail) detail)
                ((consp detail) (alist-get 'message (alist-get 'error detail)))
                ((stringp error) error)
                ((consp error) (alist-get 'message error)))))
        (and body (not (string-empty-p body)) (string-trim body)))))

(defun ai-image-gen--plz-error-message (err)
  "Return a readable message for plz error ERR."
  (let ((response (plz-error-response err))
        (curl (plz-error-curl-error err)))
    (cond
     (response
      (format "HTTP %s: %s"
              (plz-response-status response)
              (or (ai-image-gen--error-message (plz-response-body response))
                  "no details")))
     (curl (format "curl error %s: %s" (car curl) (cdr curl)))
     (t (or (plz-error-message err) "unknown error")))))

(defun ai-image-gen--file-bytes (file)
  "Return the contents of FILE as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun ai-image-gen--multipart (boundary fields files)
  "Return a multipart/form-data body as a unibyte string.
BOUNDARY separates the parts.  FIELDS is an alist of name to value,
sent as text; FILES an alist of name to file name, sent as bytes."
  (let ((text (lambda (&rest parts) (encode-coding-string (apply #'concat parts) 'utf-8))))
    (apply #'concat
           (append
            (mapcar (lambda (field)
                      (funcall text "--" boundary "\r\n"
                               "Content-Disposition: form-data; name=\"" (car field) "\"\r\n\r\n"
                               (format "%s" (cdr field)) "\r\n"))
                    fields)
            (mapcan (lambda (file)
                      (list (funcall text "--" boundary "\r\n"
                                     "Content-Disposition: form-data; name=\"" (car file)
                                     "\"; filename=\"" (file-name-nondirectory (cdr file)) "\"\r\n"
                                     "Content-Type: "
                                     (or (mailcap-file-name-to-mime-type (cdr file))
                                         "application/octet-stream")
                                     "\r\n\r\n")
                            (ai-image-gen--file-bytes (cdr file))
                            (funcall text "\r\n")))
                    files)
            (list (funcall text "--" boundary "--\r\n"))))))

(defun ai-image-gen--post (provider path content-type body then else)
  "POST BODY of CONTENT-TYPE to PATH under PROVIDER's URL.
Call THEN with the decoded images or ELSE with an error message."
  (plz 'post (concat (string-remove-suffix "/" (ai-image-gen-openai-url provider)) path)
    :headers (ai-image-gen--headers provider content-type)
    :body body
    :body-type 'binary
    :as #'json-read
    :timeout ai-image-gen-timeout
    :then (lambda (json)
            (condition-case err
                (funcall then (ai-image-gen--parse-images json))
              (ai-image-gen-error (funcall else (cadr err)))))
    :else (lambda (err) (funcall else (ai-image-gen--plz-error-message err)))))

(cl-defmethod ai-image-gen-provider-generate ((provider ai-image-gen-openai) request then else)
  (condition-case err
      (ai-image-gen--post
       provider "/images/generations" "application/json"
       (encode-coding-string (json-encode (ai-image-gen-openai--body provider request)) 'utf-8)
       then else)
    (ai-image-gen-error (funcall else (cadr err)) nil)))

(cl-defmethod ai-image-gen-provider-generate ((provider ai-image-gen-openai)
                                              (request ai-image-gen-edit-request)
                                              then else)
  (condition-case err
      (let* ((boundary (format "ai-image-gen-%s" (md5 (format "%s%s" (float-time) (random)))))
             (images (ai-image-gen-edit-request-images request))
             (image-field (if (cdr images) "image[]" "image"))
             (files (append (mapcar (lambda (file) (cons image-field file)) images)
                            (when-let* ((mask (ai-image-gen-edit-request-mask request)))
                              (list (cons "mask" mask))))))
        (ai-image-gen--post
         provider "/images/edits"
         (concat "multipart/form-data; boundary=" boundary)
         (ai-image-gen--multipart
          boundary
          (mapcar (lambda (field) (cons (symbol-name (car field)) (cdr field)))
                  (ai-image-gen-openai--body provider request))
          files)
         then else))
    (ai-image-gen-error (funcall else (cadr err)) nil)))

;;;; Entry points

(cl-defun ai-image-gen-generate (request &key provider then else)
  "Generate REQUEST asynchronously with PROVIDER (a name or object).
An `ai-image-gen-edit-request' edits its source images instead.
THEN receives a list of `ai-image-gen-image'; ELSE an error message.
ELSE defaults to reporting the error with `message'."
  (cl-check-type request ai-image-gen-request)
  (let ((provider (if (ai-image-gen-provider-p provider)
                      provider
                    (ai-image-gen-get-provider provider))))
    (ai-image-gen-provider-generate
     provider request
     (or then #'ignore)
     (or else (lambda (message) (message "Image generation failed: %s" message))))))

(cl-defun ai-image-gen-generate-sync (request &key provider)
  "Generate REQUEST with PROVIDER and wait for the images.
Return a list of `ai-image-gen-image'; signal `ai-image-gen-error'
on failure.  Quitting cancels the request."
  (let* ((done nil) (images nil) (failure nil)
         (process (ai-image-gen-generate
                   request :provider provider
                   :then (lambda (result) (setq images result done t))
                   :else (lambda (message) (setq failure message done t)))))
    (unwind-protect
        (with-timeout (ai-image-gen-timeout
                       (setq failure "timed out" done t))
          (while (not done)
            (accept-process-output nil 0.05)))
      (when (and (processp process) (process-live-p process))
        (delete-process process)))
    (if failure
        (signal 'ai-image-gen-error (list failure))
      images)))

(defun ai-image-gen-image-extension (image)
  "Return a file extension for IMAGE, without the dot."
  (pcase (ai-image-gen-image-mime-type image)
    ("image/jpeg" "jpg")
    ("image/webp" "webp")
    ("image/gif" "gif")
    (_ "png")))

(defun ai-image-gen--slug (prompt)
  "Return a short file-name slug for PROMPT."
  (let* ((words (split-string (downcase prompt) "[^[:alnum:]]+" t))
         (slug (string-join (seq-take words 6) "-")))
    (if (string-empty-p slug) "image" slug)))

(defun ai-image-gen-default-file (request image)
  "Return a fresh file in `ai-image-gen-directory' for IMAGE of REQUEST."
  (let ((base (format "%s-%s%s"
                      (format-time-string "%Y%m%d-%H%M%S")
                      (ai-image-gen--slug (ai-image-gen-request-prompt request))
                      (if-let* ((seed (ai-image-gen-image-seed image)))
                          (format "-s%d" seed)
                        ""))))
    (make-directory ai-image-gen-directory t)
    (cl-loop for i from 0
             for file = (expand-file-name
                         (format "%s%s.%s" base (if (zerop i) "" (format "-%d" i))
                                 (ai-image-gen-image-extension image))
                         ai-image-gen-directory)
             unless (file-exists-p file) return file)))

(defun ai-image-gen-image-save (image file)
  "Write IMAGE's bytes to FILE and return FILE."
  (let ((dir (file-name-directory (expand-file-name file))))
    (when dir (make-directory dir t)))
  (let ((coding-system-for-write 'binary))
    (write-region (ai-image-gen-image-data image) nil file nil 'silent))
  file)

;;;; Commands

(defun ai-image-gen--insert-link (file)
  "Insert a reference to image FILE suited to the current buffer."
  (let ((path (abbreviate-file-name file)))
    (cond
     ((derived-mode-p 'org-mode)
      (insert (format "[[file:%s]]" path))
      (cond
       ((fboundp 'org-link-preview-region)
        (org-link-preview-region nil t (line-beginning-position) (line-end-position)))
       ((fboundp 'org-display-inline-images)
        (org-display-inline-images nil t (line-beginning-position) (line-end-position)))))
     ((derived-mode-p 'message-mode)
      (insert (format "<#part type=\"%s\" filename=\"%s\" disposition=inline>\n<#/part>\n"
                      (or (mailcap-file-name-to-mime-type file) "image/png")
                      file)))
     (t (insert path)))))

;;;###autoload
(defun ai-image-gen-insert (prompt &optional provider)
  "Generate an image from PROMPT and insert a reference to it at point.
With an active region, the region is the default prompt.  With a
prefix argument, choose the PROVIDER."
  (interactive
   (list (read-string "Image prompt: "
                      (when (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (when current-prefix-arg (ai-image-gen-read-provider "Provider: "))))
  (ai-image-gen--generate-at (ai-image-gen-request-create :prompt prompt)
                             provider (point) nil))

(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-lineage "org-element-ast" (datum &optional types with-self))
(declare-function org-element-property "org-element-ast" (property node &optional dflt force-undefer))

(defun ai-image-gen--image-at-point ()
  "Return (FILE BEG END) for the image file referenced at point, or nil.
In Org buffers this is a file link; elsewhere a file name."
  (or (when (derived-mode-p 'org-mode)
        (when-let* ((link (org-element-lineage (org-element-context) '(link) t))
                    ((equal (org-element-property :type link) "file")))
          (list (expand-file-name (org-element-property :path link))
                (org-element-property :begin link)
                (save-excursion
                  (goto-char (org-element-property :end link))
                  (skip-chars-backward " \t\n")
                  (point)))))
      (when-let* ((bounds (bounds-of-thing-at-point 'filename))
                  (file (expand-file-name
                         (buffer-substring-no-properties (car bounds) (cdr bounds))))
                  ((file-regular-p file)))
        (list file (car bounds) (cdr bounds)))))

;;;###autoload
(defun ai-image-gen-edit (source prompt &optional replace provider)
  "Edit image SOURCE as PROMPT describes and insert a reference to the result.
Interactively SOURCE is the image linked at point, or read from the
minibuffer.  The result goes on the line after the reference at
point; with a prefix argument it REPLACEs the reference.  PROVIDER
defaults to `ai-image-gen-default-provider'."
  (interactive
   (let ((source (or (car (ai-image-gen--image-at-point))
                     (read-file-name "Image to edit: " nil nil t))))
     (list source
           (read-string (format "Edit %s: " (file-name-nondirectory source)))
           current-prefix-arg)))
  (let* ((request (ai-image-gen-edit-request-create :prompt prompt :images source))
         (at-point (ai-image-gen--image-at-point))
         (reference (and at-point
                         (equal (car at-point) (car (ai-image-gen-edit-request-images request)))
                         (cdr at-point))))
    (cond
     ((and replace reference)
      (ai-image-gen--generate-at request provider (car reference) (cadr reference)))
     (reference
      (ai-image-gen--generate-at request provider (cadr reference) nil "\n"))
     (t (ai-image-gen--generate-at request provider (point) nil)))))

(defun ai-image-gen--generate-at (request provider position replace-end &optional prefix)
  "Generate REQUEST with PROVIDER and insert a reference at POSITION.
When REPLACE-END is non-nil, the text from POSITION to it is replaced.
PREFIX is inserted before the reference."
  (let ((start (copy-marker position))
        (end (and replace-end (copy-marker replace-end t))))
    (message "Generating image...")
    (ai-image-gen-generate
     request :provider provider
     :then (lambda (images)
             (let ((file (ai-image-gen-image-save
                          (car images) (ai-image-gen-default-file request (car images)))))
               (if (buffer-live-p (marker-buffer start))
                   (with-current-buffer (marker-buffer start)
                     (save-excursion
                       (when end (delete-region start end))
                       (goto-char start)
                       (when prefix (insert prefix))
                       (ai-image-gen--insert-link file))
                     (message "Generated %s" (abbreviate-file-name file)))
                 (message "Generated %s (buffer gone)" (abbreviate-file-name file)))
               (set-marker start nil)
               (when end (set-marker end nil))))
     :else (lambda (message)
             (set-marker start nil)
             (when end (set-marker end nil))
             (message "Image generation failed: %s" message)))))

(provide 'ai-image-gen)
;;; ai-image-gen.el ends here
