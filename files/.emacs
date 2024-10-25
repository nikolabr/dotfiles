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

(setq auth-source-save-behavior nil)
(setq custom-safe-themes t)

(setq enable-recursive-minibuffers t)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq tab-always-indent 'complete)
(setq text-mode-ispell-word-completion nil)

(setq lexical-binding t)

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-sourcerer))

;; (require 'paredit)
;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook 'enable-paredit-mode)

(use-package paredit
  :hook ((lisp-mode . enable-paredit-mode)
	 (scheme-mode . enable-paredit-mode)))

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
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package pdf-tools
  :config
  (pdf-tools-install))

(use-package auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package direnv
  :config
  (direnv-mode))

(use-package haskell-mode
  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package org-roam
  :config
  (setq org-roam-database-connector 'sqlite-builtin)
  (setq org-roam-directory (file-truename "~/org-roam"))
  (org-roam-db-autosync-mode))

(use-package nix-mode)

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package go-mode)

(use-package lsp-bridge
  ;; Install through nix
  :straight f
  :init
  (global-lsp-bridge-mode)
  (setq lsp-bridge-nix-lsp-server "nil")
  (setq lsp-bridge-enable-with-tramp t)
  :bind (:map lsp-bridge-mode-map
	      ("C-c l e" . lsp-bridge-diagnostic-jump-next)
	      ("C-c l d" . lsp-bridge-find-def)
	      ("C-c l r" . lsp-bridge-rename)
	      ("C-c l a" . lsp-bridge-code-action)
	      ("C-c l f" . lsp-bridge-find-references)
	      ("C-c l h" . lsp-bridge-popup-documentation)
	      ("C-c l o" . lsp-bridge-workspace-list-symbols)))

(use-package magit)
(use-package htmlize)
