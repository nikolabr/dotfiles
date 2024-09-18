(setq custom-file (concat user-emacs-directory "/custom.el"))

(scroll-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(desktop-save-mode 1)
(electric-pair-mode 1)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-selected-packages
      '(paredit auctex pdf-tools which-key doom-themes ivy counsel ivy-xref hydra flycheck avy lsp-mode lsp-ivy
		cmake-mode dotnet
		bazel
		direnv magit company yasnippet yasnippet-snippets))

;; Install package if not installed
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (require 'package)
  (package-initialize)
  (package-refresh-contents) 
  (mapc #'package-install package-selected-packages))

(setq auth-source-save-behavior nil)
(setq custom-safe-themes t)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(require 'catppuccin-theme)
(load-theme 'catppuccin)

(setq catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha
(catppuccin-reload)

(require 'paredit)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

(require 'which-key)
(which-key-mode)

(require 'ivy)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)

(counsel-mode)
(ivy-mode)

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
(require 'dotnet)
;; (require 'eglot)

;; (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
;; (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
;; (define-key eglot-mode-map (kbd "C-c h") 'eldoc)
;; (define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)

(require 'lsp)
(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
(define-key lsp-mode-map (kbd "<f6>") 'xref-find-references)
;; (define-key lsp-mode-map (kbd "M-o") 'helm-lsp-workspace-symbol)
(define-key lsp-mode-map (kbd "M-o") 'lsp-ivy-workspace-symbol)

(setq lsp-keymap-prefix "C-c l")
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'csharp-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))

;; (require 'direnv)
;; (direnv-mode)

(require 'magit)

(require 'company)
(setq company-idle-delay 0.0)
(setq company-minimum-prefix-length 1)
(global-company-mode)

(set-frame-font "Iosevka 11" nil t)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)
