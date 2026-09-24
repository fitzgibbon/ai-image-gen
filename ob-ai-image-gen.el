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

;;; Code:

(require 'ob)
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
    (guidance . :any))
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

(defun ob-ai-image-gen-request (body params)
  "Return the `ai-image-gen-request' for block BODY with PARAMS."
  (let ((prompt (string-trim (org-babel-expand-body:generic body params))))
    (when (string-empty-p prompt)
      (user-error "Empty image prompt"))
    (ai-image-gen-request-create
     :prompt prompt
     :size (ob-ai-image-gen--string params :size)
     :seed (ob-ai-image-gen--number params :seed)
     :steps (ob-ai-image-gen--number params :steps)
     :guidance (ob-ai-image-gen--number params :guidance))))

(defun org-babel-execute:ai-image-gen (body params)
  "Generate the image described by BODY and return its file.
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
