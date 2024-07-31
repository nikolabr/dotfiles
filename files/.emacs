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
      '(paredit which-key adwaita-dark-theme helm helm-xref
		direnv magit company yasnippet yasnippet-snippets helm-c-yasnippet))

;; Install package if not installed
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (require 'package)
  (package-initialize)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(setq auth-source-save-behavior nil)
(setq custom-safe-themes t)

(add-hook 'after-init-hook (lambda () (load-theme 'adwaita-dark)))

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

(require 'cider)

(require 'eglot)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'clojure-mode-hook 'eglot-ensure)

(require 'direnv)
(direnv-mode)

(require 'magit)

(require 'company)
(setq company-idle-delay 0.0)
(setq company-minimum-prefix-length 1)
(global-company-mode)
