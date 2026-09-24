;;; ai-image-gen-tests.el --- Tests for ai-image-gen -*- lexical-binding: t; -*-

;;; Commentary:

;; Offline tests use a fake provider and a stubbed `plz'.  The live test
;; runs only when AI_IMAGE_GEN_LIVE_URL names an mlx-openai-server base URL
;; (and AI_IMAGE_GEN_LIVE_MODEL its image model).

;;; Code:

(require 'ert)
(require 'ai-image-gen)
(require 'ob-ai-image-gen)
(require 'org)

(defconst ai-image-gen-tests--png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")
  "A 1x1 PNG.")

(cl-defstruct (ai-image-gen-tests-fake
               (:include ai-image-gen-provider)
               (:constructor ai-image-gen-tests-fake-create))
  "Provider that answers from a timer."
  (fail nil :documentation "Error message to fail with, or nil.")
  (requests nil :documentation "Requests received, newest first."))

(cl-defmethod ai-image-gen-provider-generate ((provider ai-image-gen-tests-fake) request then else)
  (push request (ai-image-gen-tests-fake-requests provider))
  (run-at-time 0 nil
               (lambda ()
                 (if-let* ((message (ai-image-gen-tests-fake-fail provider)))
                     (funcall else message)
                   (funcall then
                            (cl-loop for i below (ai-image-gen-request-n request)
                                     collect (ai-image-gen-image-create
                                              :data ai-image-gen-tests--png
                                              :mime-type "image/png"
                                              :seed (+ (or (ai-image-gen-request-seed request) 0) i)))))))
  nil)

(defmacro ai-image-gen-tests--with-registry (&rest body)
  "Run BODY with an empty registry and a temporary image directory."
  (declare (indent 0))
  `(let ((ai-image-gen--providers nil)
         (ai-image-gen-default-provider nil)
         (ai-image-gen-directory (make-temp-file "ai-image-gen-test-" t)))
     (unwind-protect (progn ,@body)
       (delete-directory ai-image-gen-directory t))))

;;;; Registry

(ert-deftest ai-image-gen-registry-sole-provider-is-default ()
  (ai-image-gen-tests--with-registry
    (let ((fake (ai-image-gen-tests-fake-create :name "fake")))
      (ai-image-gen-register-provider fake)
      (should (eq (ai-image-gen-get-provider) fake))
      (should (eq (ai-image-gen-get-provider "fake") fake)))))

(ert-deftest ai-image-gen-registry-ambiguity-and-missing ()
  (ai-image-gen-tests--with-registry
    (should-error (ai-image-gen-get-provider) :type 'user-error)
    (ai-image-gen-register-provider (ai-image-gen-tests-fake-create :name "a"))
    (ai-image-gen-register-provider (ai-image-gen-tests-fake-create :name "b"))
    (should-error (ai-image-gen-get-provider) :type 'user-error)
    (should-error (ai-image-gen-get-provider "c") :type 'user-error)
    (let ((ai-image-gen-default-provider "b"))
      (should (equal (ai-image-gen-provider-name (ai-image-gen-get-provider)) "b")))))

(ert-deftest ai-image-gen-registry-replace-and-unregister ()
  (ai-image-gen-tests--with-registry
    (ai-image-gen-register-provider (ai-image-gen-tests-fake-create :name "a"))
    (let ((second (ai-image-gen-tests-fake-create :name "a")))
      (ai-image-gen-register-provider second)
      (should (equal (ai-image-gen-provider-names) '("a")))
      (should (eq (ai-image-gen-get-provider "a") second)))
    (ai-image-gen-unregister-provider "a")
    (should (null (ai-image-gen-provider-names)))))

(ert-deftest ai-image-gen-registry-rejects-bad-providers ()
  (ai-image-gen-tests--with-registry
    (should-error (ai-image-gen-register-provider "not a provider"))
    (should-error (ai-image-gen-register-provider (ai-image-gen-tests-fake-create)))))

;;;; Request bodies

(defun ai-image-gen-tests--full-request ()
  (ai-image-gen-request-create :prompt "a platypus" :size "512x512" :n 2
                               :seed 7 :steps 4 :guidance 1.0))

(ert-deftest ai-image-gen-openai-body-is-standard ()
  (let ((body (ai-image-gen-openai--body
               (ai-image-gen-openai-create :name "o" :model "gpt-image-1")
               (ai-image-gen-tests--full-request))))
    (should (equal (sort (mapcar #'car body) #'string<) '(model n prompt size)))
    (should (equal (alist-get 'model body) "gpt-image-1"))))

(ert-deftest ai-image-gen-mlx-body-adds-sampling ()
  (let ((body (ai-image-gen-openai--body
               (ai-image-gen-mlx-create :name "m" :model "klein")
               (ai-image-gen-tests--full-request))))
    (should (equal (alist-get 'seed body) 7))
    (should (equal (alist-get 'steps body) 4))
    (should (equal (alist-get 'guidance_scale body) 1.0))
    (should (equal (alist-get 'size body) "512x512"))))

(ert-deftest ai-image-gen-mlx-body-omits-unset ()
  (let ((body (ai-image-gen-openai--body
               (ai-image-gen-mlx-create :name "m")
               (ai-image-gen-request-create :prompt "x"))))
    (should (equal (sort (mapcar #'car body) #'string<) '(n prompt)))))

(ert-deftest ai-image-gen-headers-resolve-key-functions ()
  (let ((headers (ai-image-gen--headers
                  (ai-image-gen-openai-create :key (lambda () "sk-x")) "application/json")))
    (should (equal (alist-get "Authorization" headers nil nil #'equal) "Bearer sk-x"))
    (should (equal (alist-get "Content-Type" headers nil nil #'equal) "application/json")))
  (should-not (assoc "Authorization"
                     (ai-image-gen--headers (ai-image-gen-mlx-create) "application/json"))))

;;;; Responses

(ert-deftest ai-image-gen-parse-images ()
  (let ((images (ai-image-gen--parse-images
                 `((data . [((b64_json . ,(base64-encode-string ai-image-gen-tests--png))
                             (seed . 7))])))))
    (should (= (length images) 1))
    (should (equal (ai-image-gen-image-mime-type (car images)) "image/png"))
    (should (equal (ai-image-gen-image-data (car images)) ai-image-gen-tests--png))
    (should (equal (ai-image-gen-image-seed (car images)) 7))))

(ert-deftest ai-image-gen-parse-images-requires-b64 ()
  (should-error (ai-image-gen--parse-images '((data . [((url . "https://x"))])))
                :type 'ai-image-gen-error))

(ert-deftest ai-image-gen-error-message-shapes ()
  (should (equal (ai-image-gen--error-message "{\"detail\":\"height 128 is not supported\"}")
                 "height 128 is not supported"))
  (should (equal (ai-image-gen--error-message
                  "{\"detail\":{\"error\":{\"message\":\"Model 'nope' not found\"}}}")
                 "Model 'nope' not found"))
  (should (equal (ai-image-gen--error-message "{\"error\":{\"message\":\"Invalid size\"}}")
                 "Invalid size"))
  (should (equal (ai-image-gen--error-message "Bad Gateway") "Bad Gateway"))
  (should (null (ai-image-gen--error-message nil))))

(ert-deftest ai-image-gen-sniff-mime-types ()
  (should (equal (ai-image-gen--sniff-mime-type ai-image-gen-tests--png) "image/png"))
  (should (equal (ai-image-gen--sniff-mime-type "\xff\xd8\xff\xe0rest") "image/jpeg"))
  (should (equal (ai-image-gen--sniff-mime-type "RIFF\0\0\0\0WEBPVP8 ") "image/webp"))
  (should (equal (ai-image-gen--sniff-mime-type "nope") "application/octet-stream")))

(ert-deftest ai-image-gen-openai-generate-posts-and-decodes ()
  (let (seen)
    (cl-letf (((symbol-function 'plz)
               (lambda (method url &rest args)
                 (setq seen (list method url args))
                 (funcall (plist-get args :then)
                          `((data . [((b64_json . ,(base64-encode-string ai-image-gen-tests--png))
                                      (seed . 3))])))
                 nil)))
      (let (images)
        (ai-image-gen-provider-generate
         (ai-image-gen-mlx-create :name "m" :url "https://host:1/v1/" :model "klein")
         (ai-image-gen-request-create :prompt "café" :seed 3)
         (lambda (result) (setq images result))
         (lambda (message) (error "Unexpected failure: %s" message)))
        (should (eq (nth 0 seen) 'post))
        (should (equal (nth 1 seen) "https://host:1/v1/images/generations"))
        (let ((body (json-parse-string (decode-coding-string (plist-get (nth 2 seen) :body) 'utf-8)
                                       :object-type 'alist)))
          (should (equal (alist-get 'prompt body) "café"))
          (should (equal (alist-get 'seed body) 3)))
        (should (equal (ai-image-gen-image-seed (car images)) 3))))))

(ert-deftest ai-image-gen-openai-generate-reports-http-errors ()
  (cl-letf (((symbol-function 'plz)
             (lambda (_method _url &rest args)
               (funcall (plist-get args :else)
                        (make-plz-error
                         :response (make-plz-response
                                    :status 400
                                    :body "{\"detail\":\"height 128 is not supported\"}")))
               nil)))
    (let (failure)
      (ai-image-gen-provider-generate
       (ai-image-gen-mlx-create :name "m")
       (ai-image-gen-request-create :prompt "x" :size "4096x128")
       #'ignore
       (lambda (message) (setq failure message)))
      (should (equal failure "HTTP 400: height 128 is not supported")))))

;;;; Edits

(defmacro ai-image-gen-tests--with-source-images (names &rest body)
  "Bind NAMES to fresh PNG files for BODY."
  (declare (indent 1))
  `(let ,(mapcar (lambda (name)
                   `(,name (let ((file (make-temp-file ,(format "ai-image-gen-%s-" name) nil ".png")))
                             (let ((coding-system-for-write 'binary))
                               (write-region ai-image-gen-tests--png nil file nil 'silent))
                             file)))
                 names)
     (unwind-protect (progn ,@body)
       (dolist (file (list ,@names)) (delete-file file)))))

(defun ai-image-gen-tests--multipart-parts (body boundary)
  "Split multipart BODY on BOUNDARY into (HEADERS . CONTENT) pairs."
  (let ((delimiter (concat "--" boundary)))
    (should (string-suffix-p (concat delimiter "--\r\n") body))
    (mapcar (lambda (part)
              (let ((split (string-search "\r\n\r\n" part)))
                (cons (substring part 0 split)
                      (substring part (+ split 4) (- (length part) 2)))))
            (seq-filter (lambda (part) (not (member part '("" "--\r\n"))))
                        (mapcar (lambda (part) (string-remove-prefix "\r\n" part))
                                (split-string body (regexp-quote delimiter)))))))

(defun ai-image-gen-tests--capture-edit (provider request)
  "Send REQUEST to PROVIDER with `plz' stubbed; return (URL HEADERS PARTS FAILURE)."
  (let (url headers body failure)
    (cl-letf (((symbol-function 'plz)
               (lambda (_method target &rest args)
                 (setq url target
                       headers (plist-get args :headers)
                       body (plist-get args :body))
                 (funcall (plist-get args :then)
                          `((data . [((b64_json . ,(base64-encode-string ai-image-gen-tests--png)))])))
                 nil)))
      (ai-image-gen-provider-generate provider request #'ignore
                                      (lambda (message) (setq failure message))))
    (list url headers
          (let ((content-type (alist-get "Content-Type" headers nil nil #'equal)))
            (when (and body (string-match "boundary=\\(.+\\)" content-type))
              (should-not (multibyte-string-p body))
              (ai-image-gen-tests--multipart-parts body (match-string 1 content-type))))
          failure)))

(defun ai-image-gen-tests--part (parts name)
  "Return the content of the part called NAME in PARTS."
  (cdr (seq-find (lambda (part)
                   (string-match-p (format "name=\"%s\"" (regexp-quote name)) (car part)))
                 parts)))

(ert-deftest ai-image-gen-edit-request-validates-sources ()
  (should-error (ai-image-gen-edit-request-create :prompt "x") :type 'user-error)
  (should-error (ai-image-gen-edit-request-create :prompt "x" :images "/no/such.png")
                :type 'user-error)
  (ai-image-gen-tests--with-source-images (source)
    (should-error (ai-image-gen-edit-request-create :prompt "x" :images source
                                                    :mask "/no/mask.png")
                  :type 'user-error)
    (let ((request (ai-image-gen-edit-request-create :prompt "x" :images source :seed 3)))
      (should (ai-image-gen-request-p request))
      (should (equal (ai-image-gen-edit-request-images request) (list source)))
      (should (equal (ai-image-gen-request-seed request) 3)))))

(ert-deftest ai-image-gen-multipart-keeps-bytes ()
  (ai-image-gen-tests--with-source-images (source)
    (let* ((body (ai-image-gen--multipart "B0UND" '(("prompt" . "café") ("n" . 1))
                                          `(("image" . ,source))))
           (parts (ai-image-gen-tests--multipart-parts body "B0UND")))
      (should-not (multibyte-string-p body))
      (should (= (length parts) 3))
      (should (equal (decode-coding-string (ai-image-gen-tests--part parts "prompt") 'utf-8) "café"))
      (should (equal (ai-image-gen-tests--part parts "n") "1"))
      (should (equal (ai-image-gen-tests--part parts "image") ai-image-gen-tests--png))
      (should (string-match-p "Content-Type: image/png"
                              (car (seq-find (lambda (part) (string-match-p "filename=" (car part)))
                                             parts)))))))

(ert-deftest ai-image-gen-openai-edit-uploads-images-and-mask ()
  (ai-image-gen-tests--with-source-images (first second mask)
    (pcase-let ((`(,url ,headers ,parts ,failure)
                 (ai-image-gen-tests--capture-edit
                  (ai-image-gen-openai-create :name "o" :url "https://api.example/v1" :model "gpt-image-1")
                  (ai-image-gen-edit-request-create :prompt "add a hat" :n 2
                                                    :images (list first second) :mask mask))))
      (should-not failure)
      (should (equal url "https://api.example/v1/images/edits"))
      (should (string-prefix-p "multipart/form-data; boundary="
                               (alist-get "Content-Type" headers nil nil #'equal)))
      (should (= (seq-count (lambda (part) (string-match-p "name=\"image\\[\\]\"" (car part))) parts)
                 2))
      (should (equal (ai-image-gen-tests--part parts "mask") ai-image-gen-tests--png))
      (should (equal (ai-image-gen-tests--part parts "prompt") "add a hat"))
      (should (equal (ai-image-gen-tests--part parts "model") "gpt-image-1"))
      (should (equal (ai-image-gen-tests--part parts "n") "2")))))

(ert-deftest ai-image-gen-mlx-edit-sends-sampling-without-n ()
  (ai-image-gen-tests--with-source-images (source)
    (pcase-let ((`(,url ,_headers ,parts ,failure)
                 (ai-image-gen-tests--capture-edit
                  (ai-image-gen-mlx-create :name "m" :url "https://host:1/v1" :model "klein")
                  (ai-image-gen-edit-request-create :prompt "wrench" :images source
                                                    :seed 11 :steps 4 :guidance 1.0))))
      (should-not failure)
      (should (equal url "https://host:1/v1/images/edits"))
      (should (ai-image-gen-tests--part parts "image"))
      (should (equal (ai-image-gen-tests--part parts "seed") "11"))
      (should (equal (ai-image-gen-tests--part parts "steps") "4"))
      (should (equal (ai-image-gen-tests--part parts "guidance_scale") "1.0"))
      (should-not (ai-image-gen-tests--part parts "n")))))

(ert-deftest ai-image-gen-mlx-edit-rejects-mask-and-batches ()
  (ai-image-gen-tests--with-source-images (source mask)
    (let ((provider (ai-image-gen-mlx-create :name "m")))
      (should (string-match-p "mask"
                              (nth 3 (ai-image-gen-tests--capture-edit
                                      provider (ai-image-gen-edit-request-create
                                                :prompt "x" :images source :mask mask)))))
      (should (string-match-p "one image"
                              (nth 3 (ai-image-gen-tests--capture-edit
                                      provider (ai-image-gen-edit-request-create
                                                :prompt "x" :images source :n 2))))))))

(ert-deftest ai-image-gen-plain-request-still-generates ()
  (should (equal (car (ai-image-gen-tests--capture-edit
                       (ai-image-gen-mlx-create :name "m" :url "https://host:1/v1")
                       (ai-image-gen-request-create :prompt "x")))
                 "https://host:1/v1/images/generations")))

(defun ai-image-gen-tests--wait-for (predicate)
  "Spin the event loop until PREDICATE returns non-nil."
  (with-timeout (5 (error "Timed out waiting"))
    (while (not (funcall predicate))
      (accept-process-output nil 0.01))))

(ert-deftest ai-image-gen-edit-command-inserts-after-or-replaces ()
  (ai-image-gen-tests--with-registry
    (ai-image-gen-tests--with-source-images (source)
      (let ((fake (ai-image-gen-tests-fake-create :name "fake")))
        (ai-image-gen-register-provider fake)
        (dolist (replace '(nil t))
          (with-temp-buffer
            (org-mode)
            (insert (format "Look: [[file:%s]] done" source))
            (search-backward "[[file:")
            (forward-char 3)
            (ai-image-gen-edit source "add a hat" replace)
            (ai-image-gen-tests--wait-for
             (lambda () (save-excursion
                          (goto-char (point-min))
                          (search-forward (abbreviate-file-name ai-image-gen-directory) nil t))))
            (should (= (how-many "\\[\\[file:" (point-min) (point-max)) (if replace 1 2)))
            (let ((request (car (ai-image-gen-tests-fake-requests fake))))
              (should (ai-image-gen-edit-request-p request))
              (should (equal (ai-image-gen-edit-request-images request) (list source))))
            (goto-char (point-min))
            (if replace
                (should-not (search-forward source nil t))
              (should (search-forward (format "[[file:%s]]\n[[file:" source) nil t)))))))))

;;;; Entry points

(ert-deftest ai-image-gen-generate-sync-returns-images ()
  (ai-image-gen-tests--with-registry
    (ai-image-gen-register-provider (ai-image-gen-tests-fake-create :name "fake"))
    (let ((images (ai-image-gen-generate-sync
                   (ai-image-gen-request-create :prompt "x" :n 2 :seed 10))))
      (should (equal (mapcar #'ai-image-gen-image-seed images) '(10 11))))))

(ert-deftest ai-image-gen-generate-sync-signals-failure ()
  (ai-image-gen-tests--with-registry
    (ai-image-gen-register-provider (ai-image-gen-tests-fake-create :name "fake" :fail "boom"))
    (should (equal (cdr (should-error (ai-image-gen-generate-sync
                                       (ai-image-gen-request-create :prompt "x"))
                                      :type 'ai-image-gen-error))
                   '("boom")))))

(ert-deftest ai-image-gen-default-file-is-fresh ()
  (ai-image-gen-tests--with-registry
    (let* ((request (ai-image-gen-request-create :prompt "A Platypus, in a hat!"))
           (image (ai-image-gen-image-create :data ai-image-gen-tests--png
                                             :mime-type "image/png" :seed 5))
           (first (ai-image-gen-image-save image (ai-image-gen-default-file request image)))
           (second (ai-image-gen-default-file request image)))
      (should (string-match-p "-a-platypus-in-a-hat-s5\\.png\\'" first))
      (should (file-exists-p first))
      (should-not (equal first second))
      (should (equal (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally first)
                       (buffer-string))
                     ai-image-gen-tests--png)))))

;;;; Babel

(defun ai-image-gen-tests--execute (block)
  "Execute Org source BLOCK and return the results text."
  (with-temp-buffer
    (let ((org-confirm-babel-evaluate nil)
          (org-babel-load-languages '((ai-image-gen . t))))
      (org-mode)
      (insert block)
      (goto-char (point-min))
      (org-babel-execute-src-block)
      (goto-char (org-babel-where-is-src-block-result))
      (buffer-substring-no-properties (point) (point-max)))))

(ert-deftest ob-ai-image-gen-links-generated-file ()
  (ai-image-gen-tests--with-registry
    (let ((fake (ai-image-gen-tests-fake-create :name "fake")))
      (ai-image-gen-register-provider fake)
      (let ((results (ai-image-gen-tests--execute
                      "#+begin_src ai-image-gen :size 512x512 :seed 9 :steps 4\nA platypus\n#+end_src\n")))
        (should (string-match "\\[\\[file:\\([^]]+\\)\\]\\]" results))
        (should (file-exists-p (expand-file-name (match-string 1 results))))
        (let ((request (car (ai-image-gen-tests-fake-requests fake))))
          (should (equal (ai-image-gen-request-prompt request) "A platypus"))
          (should (equal (ai-image-gen-request-size request) "512x512"))
          (should (equal (ai-image-gen-request-seed request) 9))
          (should (equal (ai-image-gen-request-steps request) 4)))))))

(ert-deftest ob-ai-image-gen-honours-file-and-provider ()
  (ai-image-gen-tests--with-registry
    (ai-image-gen-register-provider (ai-image-gen-tests-fake-create :name "a"))
    (let ((b (ai-image-gen-tests-fake-create :name "b"))
          (target (expand-file-name "out/pic.png" ai-image-gen-directory)))
      (ai-image-gen-register-provider b)
      (let ((results (ai-image-gen-tests--execute
                      (format "#+begin_src ai-image-gen :provider b :file %s\nA hat\n#+end_src\n"
                              target))))
        (should (file-exists-p target))
        (should (string-match-p (regexp-quote "pic.png") results))
        (should (= (length (ai-image-gen-tests-fake-requests b)) 1))))))

(ert-deftest ob-ai-image-gen-rejects-empty-prompt ()
  (should-error (ob-ai-image-gen-request "  \n" nil) :type 'user-error))

(ert-deftest ob-ai-image-gen-edits-a-file ()
  (ai-image-gen-tests--with-registry
    (ai-image-gen-tests--with-source-images (source)
      (let ((fake (ai-image-gen-tests-fake-create :name "fake")))
        (ai-image-gen-register-provider fake)
        (ai-image-gen-tests--execute
         (format "#+begin_src ai-image-gen :image %s :seed 4\nAdd a hat\n#+end_src\n" source))
        (let ((request (car (ai-image-gen-tests-fake-requests fake))))
          (should (ai-image-gen-edit-request-p request))
          (should (equal (ai-image-gen-edit-request-images request) (list source)))
          (should (equal (ai-image-gen-request-prompt request) "Add a hat")))))))

(ert-deftest ob-ai-image-gen-edits-a-named-block-without-rerunning-it ()
  (ai-image-gen-tests--with-registry
    (let ((fake (ai-image-gen-tests-fake-create :name "fake")))
      (ai-image-gen-register-provider fake)
      (with-temp-buffer
        (let ((org-confirm-babel-evaluate nil)
              (org-babel-load-languages '((ai-image-gen . t))))
          (org-mode)
          (insert "#+name: mascot\n#+begin_src ai-image-gen :seed 7\nA platypus\n#+end_src\n\n"
                  "#+begin_src ai-image-gen :image mascot\nNow with a wrench\n#+end_src\n")
          (goto-char (point-min))
          (org-babel-execute-src-block)
          (let ((mascot (progn (goto-char (org-babel-where-is-src-block-result))
                               (forward-line)
                               (ob-ai-image-gen--link-file-at-point))))
            (should (file-exists-p mascot))
            (search-forward "Now with a wrench")
            (org-babel-execute-src-block)
            (should (= (length (ai-image-gen-tests-fake-requests fake)) 2))
            (should (equal (ai-image-gen-edit-request-images
                            (car (ai-image-gen-tests-fake-requests fake)))
                           (list mascot)))))))))

(ert-deftest ob-ai-image-gen-runs-a-named-block-with-no-result ()
  (ai-image-gen-tests--with-registry
    (let ((fake (ai-image-gen-tests-fake-create :name "fake")))
      (ai-image-gen-register-provider fake)
      (with-temp-buffer
        (let ((org-confirm-babel-evaluate nil)
              (org-babel-load-languages '((ai-image-gen . t))))
          (org-mode)
          (insert "#+name: mascot\n#+begin_src ai-image-gen\nA platypus\n#+end_src\n\n"
                  "#+begin_src ai-image-gen :image mascot\nNow with a wrench\n#+end_src\n")
          (search-backward "Now with a wrench")
          (org-babel-execute-src-block)
          (let ((requests (ai-image-gen-tests-fake-requests fake)))
            (should (= (length requests) 2))
            (should-not (ai-image-gen-edit-request-p (cadr requests)))
            (should (ai-image-gen-edit-request-p (car requests)))))))))

(ert-deftest ob-ai-image-gen-edits-a-named-link-and-several-sources ()
  (ai-image-gen-tests--with-registry
    (ai-image-gen-tests--with-source-images (logo photo)
      (let ((fake (ai-image-gen-tests-fake-create :name "fake")))
        (ai-image-gen-register-provider fake)
        (with-temp-buffer
          (let ((org-confirm-babel-evaluate nil)
                (org-babel-load-languages '((ai-image-gen . t))))
            (org-mode)
            (insert (format "#+name: logo\n[[file:%s]]\n\n#+begin_src ai-image-gen :image logo %s\nCombine them\n#+end_src\n"
                            logo photo))
            (search-backward "Combine them")
            (org-babel-execute-src-block)))
        (should (equal (ai-image-gen-edit-request-images
                        (car (ai-image-gen-tests-fake-requests fake)))
                       (list logo photo)))))))

(ert-deftest ob-ai-image-gen-rejects-bad-sources ()
  (should-error (ob-ai-image-gen-request "x" '((:image . "no-such-thing"))) :type 'user-error)
  (should-error (ob-ai-image-gen-request "x" '((:mask . "no-such-thing"))) :type 'user-error))

;;;; Live

(ert-deftest ai-image-gen-live-mlx ()
  "Generate one small image against a real mlx-openai-server."
  (let ((url (getenv "AI_IMAGE_GEN_LIVE_URL")))
    (skip-unless url)
    (let* ((provider (ai-image-gen-mlx-create
                      :name "live" :url url
                      :model (or (getenv "AI_IMAGE_GEN_LIVE_MODEL")
                                 "black-forest-labs/FLUX.2-klein-9B")))
           (images (ai-image-gen-generate-sync
                    (ai-image-gen-request-create :prompt "a cheerful platypus mascot, flat vector"
                                                 :size "512x512" :seed 7)
                    :provider provider)))
      (should (= (length images) 1))
      (should (equal (ai-image-gen-image-mime-type (car images)) "image/png"))
      (should (equal (ai-image-gen-image-seed (car images)) 7))
      (should-error (ai-image-gen-generate-sync
                     (ai-image-gen-request-create :prompt "x" :size "4096x128")
                     :provider provider)
                    :type 'ai-image-gen-error)
      (let ((source (make-temp-file "ai-image-gen-live-" nil ".png")))
        (unwind-protect
            (let ((edited (progn
                            (ai-image-gen-image-save (car images) source)
                            (ai-image-gen-generate-sync
                             (ai-image-gen-edit-request-create
                              :prompt "the same platypus mascot, now wearing a red scarf"
                              :images source :seed 7)
                             :provider provider))))
              (should (= (length edited) 1))
              (should (equal (ai-image-gen-image-mime-type (car edited)) "image/png"))
              (should (equal (ai-image-gen-image-seed (car edited)) 7))
              (should-not (equal (ai-image-gen-image-data (car edited))
                                 (ai-image-gen-image-data (car images)))))
          (delete-file source))))))

(provide 'ai-image-gen-tests)
;;; ai-image-gen-tests.el ends here
