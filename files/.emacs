(setq custom-file (concat user-emacs-directory "/custom.el"))

(scroll-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(desktop-save-mode 1)
(electric-pair-mode 1)

(setq package-archives nil)

(setq auth-source-save-behavior nil)
(setq custom-safe-themes t)

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-sourcerer)

(require 'paredit)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

(require 'which-key)
(which-key-mode)

(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(helm-autoresize-mode t)

(helm-mode)

(require 'yasnippet)
(global-set-key (kbd "C-c y") 'company-yasnippet)
(yas-global-mode 1)

;; (require 'cider)

(require 'pdf-tools)
(pdf-tools-install)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(require 'cmake-mode)

(require 'eglot)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'clojure-mode-hook 'eglot-ensure)
(add-hook 'csharp-mode-hook 'eglot-ensure)

(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
(define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)

(require 'direnv)
(direnv-mode)

(require 'magit)

(require 'company)
(setq company-idle-delay 0.0)
(setq company-minimum-prefix-length 1)
(global-company-mode)
