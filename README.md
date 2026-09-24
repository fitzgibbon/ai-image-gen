# ai-image-gen

`ai-image-gen` generates images from Emacs through a small registry of
providers, and `ob-ai-image-gen` makes that available as an Org Babel
language whose blocks are prompts and whose results are image links.

Two provider types are built in:

- `ai-image-gen-openai` — the OpenAI images API (`/v1/images/generations`),
  or any server speaking it.
- `ai-image-gen-mlx` — [mlx-openai-server](https://github.com/cubist38/mlx-openai-server)'s
  variant, which also accepts `seed`, `steps` and `guidance_scale` and
  reports the seed of each image.

Requests go through [plz](https://github.com/alphapapa/plz.el), so they use
curl and honour `CURL_CA_BUNDLE` for self-signed local endpoints.

## Setup

```elisp
(use-package ob-ai-image-gen
  :vc (:url "https://github.com/fitzgibbon/ai-image-gen.git")
  :demand t
  :config
  (ai-image-gen-register-provider
   (ai-image-gen-mlx-create
    :name "flux2-klein"
    :url "https://localhost:11490/v1"
    :model "black-forest-labs/FLUX.2-klein-9B"))
  (ai-image-gen-register-provider
   (ai-image-gen-openai-create
    :name "gpt-image"
    :key #'my/get-openai-api-key
    :model "gpt-image-1"))
  (setq ai-image-gen-default-provider "flux2-klein")
  (add-to-list 'org-babel-load-languages '(ai-image-gen . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
```

`:key` may be a string or a function returning one; it is sent as a bearer
token. When exactly one provider is registered it is the default.

Images without an explicit destination go to `ai-image-gen-directory`.

## Org Babel

The block body is the prompt:

```org
#+begin_src ai-image-gen :size 768x512 :seed 7
A platypus in a yellow hard hat giving a thumbs up, flat vector illustration
#+end_src

#+RESULTS:
[[file:~/.emacs.d/ai-image-gen/20260923-234814-a-platypus-in-a-yellow-s7.png]]
```

Header arguments:

| argument    | meaning                                              |
|-------------|------------------------------------------------------|
| `:provider` | registered provider name (default provider if unset) |
| `:size`     | `WIDTHxHEIGHT`                                       |
| `:seed`     | seed (`ai-image-gen-mlx` only)                       |
| `:steps`    | inference steps (`ai-image-gen-mlx` only)            |
| `:guidance` | guidance scale (`ai-image-gen-mlx` only)             |
| `:file`     | where to save the image                              |

## Commands

`M-x ai-image-gen-insert` prompts for a description (the active region is the
default), generates asynchronously and inserts a reference at point: an Org
link with an inline preview in Org buffers, an inline MML image part in
message buffers, the file name elsewhere. With a prefix argument it asks
which provider to use.

## Lisp API

```elisp
(let ((request (ai-image-gen-request-create
                :prompt "a lighthouse at dusk" :size "512x512" :n 2)))
  (ai-image-gen-generate
   request
   :provider "flux2-klein"
   :then (lambda (images)
           (dolist (image images)
             (ai-image-gen-image-save image (ai-image-gen-default-file request image))))
   :else (lambda (message) (message "failed: %s" message))))
```

`ai-image-gen-generate-sync` returns the list of `ai-image-gen-image` objects
instead, signalling `ai-image-gen-error` on failure. Each image carries its
bytes, sniffed MIME type and, where the provider reports one, its seed.

## Adding a provider

A provider is a struct that includes `ai-image-gen-provider` and implements
`ai-image-gen-provider-generate`:

```elisp
(cl-defstruct (my-provider (:include ai-image-gen-provider)
                           (:constructor my-provider-create))
  endpoint)

(cl-defmethod ai-image-gen-provider-generate ((provider my-provider) request then else)
  ;; Call THEN with a list of `ai-image-gen-image', or ELSE with a message.
  ;; Return the request process (or nil) so callers can cancel it.
  )
```

Providers speaking a variant of the OpenAI API can instead include
`ai-image-gen-openai` and specialise `ai-image-gen-openai--body`, as
`ai-image-gen-mlx` does.

## Tests

```sh
emacs --batch -L . -L tests -l ai-image-gen-tests -f ert-run-tests-batch-and-exit
```

`plz` must be on the load path. The live test is skipped unless
`AI_IMAGE_GEN_LIVE_URL` names an mlx-openai-server base URL (and optionally
`AI_IMAGE_GEN_LIVE_MODEL` its image model):

```sh
AI_IMAGE_GEN_LIVE_URL=https://localhost:11490/v1 \
  emacs --batch -L . -L tests -l ai-image-gen-tests -f ert-run-tests-batch-and-exit
```
