(use-modules
  (gnu home)
  (gnu packages)
  (gnu services)
  (guix gexp)
  (gnu home services shells))

(home-environment
  (packages
    (specifications->packages
     (list "blender"
           "qutebrowser"
           "ungoogled-chromium"
           "mpv"
           "yt-dlp"
           "notmuch"
           "msmtp"
           "feh"
           "wine64"
           "l2md"
           "dunst"
           "unzip"
           "xset"
           "xdg-utils"

           ;; Virt manager
           "virt-manager"
           "dconf" ;; Needed for virt-manager (See Guix note)
           "adwaita-icon-theme"
           "hicolor-icon-theme"
           "qemu"

	   "obs"
	   "kdenlive"

           "git"
           "openssh"
           "node"
           "flameshot"
           "stow"
           "isync"
           "alacritty"
           ;; "pinentry-tty"
	   "pinentry"
           "password-store"
           "gnupg"
           "alsa-utils"
           "unclutter"
           "xmodmap"
           "xcape"
           "pulsemixer"
           "font-jetbrains-mono"
           "font-openmoji"
           "libvterm"
           "picom"

           ;; Emacs Packages
           "emacs"
           "ispell"
           "emacs-guix"
           "emacs-gnus-alias"
           "emacs-general"
           "emacs-elfeed"
	   "emacs-ement"
	   "curl"

           "emacs-notmuch"
           "emacs-undo-tree"
           "emacs-evil"
           "emacs-evil-collection"
           "emacs-doom-themes"
           "emacs-visual-fill-column"
           "emacs-vertico"
           "emacs-marginalia"
           "emacs-org-auto-tangle"
           "emacs-rainbow-delimiters"
           "emacs-all-the-icons"
           "emacs-all-the-icons-completion"
           "emacs-doom-modeline"
           "emacs-which-key"
           "emacs-diminish"
           "emacs-counsel"
           "emacs-hydra"
           "emacs-no-littering"
           "emacs-prettier"
           "emacs-org"
           "pandoc"
           "emacs-org-superstar"
           "emacs-org-bullets"
           "emacs-ox-pandoc"
           "emacs-org-appear"
           "emacs-olivetti"
           "emacs-org-super-agenda"
           "emacs-evil-org"
           "emacs-org-reveal"
           "emacs-org-roam"
           "gcc-toolchain"
           "emacs-org-make-toc"
           "emacs-erc-hl-nicks"
           "emacs-erc-image"
           "emacs-emojify"
           "emacs-org-mime"
           "emacs-projectile"
           "emacs-counsel-projectile"
           "emacs-yasnippet"
           "emacs-magit"
           "emacs-sudo-edit"
           "emacs-lsp-mode"
           "emacs-dap-mode"
           "emacs-lsp-ui"
           "emacs-lsp-treemacs"
           "emacs-ccls" ; C++
	   "ccls"
           "emacs-geiser-guile" ; Guile Scheme
           "emacs-geiser"
           "emacs-smartparens"
           "emacs-company"
           "emacs-vterm"
           "emacs-password-store"
           "emacs-consult")))
  (services
    (list (service
            home-bash-service-type
            (home-bash-configuration
              (aliases
                '(("grep" . "grep --color=auto")
                  ("ll" . "ls -l")
                  ("ls" . "ls -p --color=auto")))
              (bashrc
                (list (local-file
                        "/home/haider/.config/guix/.bashrc"
                        "bashrc")))
              (bash-profile
                (list (local-file
                        "/home/haider/.config/guix/.bash_profile"
                        "bash_profile"))))))))
