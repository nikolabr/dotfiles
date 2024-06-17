;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
	     (gnu home services)
	     (gnu home services dotfiles)
	     
	     (gnu packages)
	     
	     (gnu services)
	     (guix gexp)
	     (gnu home services shells))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
 (packages (specifications->packages (list
				      "htop" "neofetch"
				      
				      "librewolf" "icecat" "qbittorrent" "torbrowser"
				      "gajim" "gajim-omemo" "element-desktop"

				      "steam"
				      
				      "ibus" "ibus-anthy" "gnome-shell-extension-customize-ibus"
				      "font-google-noto" "font-google-noto-sans-cjk" "font-google-noto-serif-cjk" "font-google-noto-emoji"
				      "font-adobe-source-han-sans" "font-ipa"

				      "clang-toolchain"
				      "python-lsp-server"
				      
				      "flatpak"
				      "xdg-desktop-portal" "xdg-desktop-portal-gtk" "gnome-tweaks"
				      
				      "jmtpfs"
				      "nsxiv" "mpv" "direnv"
				      "keepassxc"
				      
				      "emacs-paredit"
				      "emacs-which-key"
				      "emacs-adwaita-dark-theme"
				      "emacs-helm" "emacs-helm-xref"
				      "emacs-direnv"
				      "emacs-magit"
				      "emacs-company"
				      "emacs-yasnippet" "emacs-yasnippet-snippets" "emacs-helm-c-yasnippet"

				      "emacs-guix"
				      "emacs-pgtk")))
 
 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 (services
  (list
   (service home-dotfiles-service-type
	    (home-dotfiles-configuration
	     (directories (list "files/" ))))
   (service home-bash-service-type
                 (home-bash-configuration
                  (aliases '(("emacskill" . "emacsclient -e \"(save-buffers-kill-emacs)\"")))
		  (environment-variables
		   `(
		     ("GTK_IM_MODULE" . "ibus")
		     ("QT_IM_MODULE" . "ibus")
		     ("XMODIFIERS" . "@im=ibus")
		     ;; ("GTK_THEME" . "Adwaita:dark")
		     ))
                  (bashrc (list (local-file ".bashrc" "bashrc")))
                  (bash-profile (list (local-file ".bash_profile"
                                                  "bash_profile")))
                  (bash-logout (list (local-file ".bash_logout"
                                                 "bash_logout"))))))))
