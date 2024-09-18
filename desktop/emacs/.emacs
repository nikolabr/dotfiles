(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq custom-file (concat user-emacs-directory "/custom.el"))

(scroll-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(desktop-save-mode 1)
(electric-pair-mode 1)
(savehist-mode 1)
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
(set-frame-font "Iosevka 11" nil t)

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package catppuccin-theme
  :custom
  (catppucin-flavor 'macchiato)
  :init
  (load-theme 'catppuccin))

(use-package vertico
  :straight t
  :init (vertico-mode))

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic partial-completion))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :bind
  (
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   
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
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  :init (global-corfu-mode))

(use-package lsp-mode
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  :bind-keymap ("C-c l" . lsp-command-map))

(use-package cmake-mode)
(use-package magit)