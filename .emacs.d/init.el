(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq custom-file (concat user-emacs-directory "/custom.el"))

(scroll-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(desktop-save-mode 1)
(electric-pair-mode 1)
(recentf-mode 1)

(setq auth-source-save-behavior nil)
(setq custom-safe-themes t)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(setq enable-recursive-minibuffers t)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq tab-always-indent 'complete)
(setq text-mode-ispell-word-completion nil)
(setq lexical-binding t)

;; (set-frame-font "Iosevka 11" nil t)

(use-package catppuccin-theme
  :ensure t
  :custom
  (catppucin-flavor 'macchiato)
  :init
  (load-theme 'catppuccin))

(use-package project
  :ensure t)

(use-package transient
  :ensure t)

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic partial-completion))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind
  (
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("C-c h" . consult-history)
   ("C-x b" . consult-buffer)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer))
  
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :init (global-corfu-mode))

(use-package paredit
  :ensure t
  :hook
  (lisp-mode . paredit-mode))

(use-package eglot
  :ensure nil
  :hook
  (c++-mode . 'eglot-ensure))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode))

(use-package flymake
  :ensure t)

(use-package cmake-mode
  :ensure t)
(use-package magit
  :ensure t)
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package realgud
  :ensure t)
