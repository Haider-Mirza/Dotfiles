;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
 (gnu home)
 (gnu packages)
 (gnu services)
 (guix gexp)
 (gnu home services shells))

(home-environment
 (packages
  (map (compose list specification->package+output)
       (list "blender"
             "qutebrowser"
             "mpv"
             "dunst"
             "qemu"
             "virt-manager"
             "git"
             "rofi"
             "node"
             "flameshot"
             "polybar"
             "libvirt"
             "isync"
             "alacritty"
             "pandoc"
             "pinentry-tty"
             "password-store"
             "gnupg"
             "unclutter"
             "xmodmap"
             "alsa-utils"
             "font-jetbrains-mono"
             "libvterm"
             "slock"
             "picom"
             "feh"

	     ;; Emacs Packages
	     "emacs"
	     "emacs-guix"
	     "emacs-general"
	     "emacs-undo-tree"
	     ;; "emacs-dashboard"
	     "emacs-evil"
	     "emacs-evil-collection"
	     "emacs-doom-themes"
	     "emacs-visual-fill-column"
	     "emacs-vertico"
	     ;; "emacs-savehist"
	     "emacs-marginalia"
	     "emacs-rainbow-delimiters"
	     "emacs-all-the-icons"
	     "emacs-all-the-icons-completion"
	     "emacs-doom-modeline"
	     "emacs-which-key"
	     "emacs-diminish"
	     "emacs-counsel"
	     "emacs-helpful"
	     "emacs-hydra"
	     "emacs-no-littering"
	     "emacs-prettier"
	     "emacs-org"
	     "emacs-org-superstar"
	     "emacs-org-bullets"
	     "emacs-ox-pandoc"
	     "emacs-org-appear"
	     "emacs-org-super-agenda"
	     "emacs-evil-org"
	     "emacs-org-reveal"
	     "emacs-org-roam"
	     "emacs-org-make-toc"
	     "emacs-erc-hl-nicks"
	     "emacs-erc-image"
	     "emacs-emojify"
	     ;; "emacs-unicode-fonts"
	     ;; "emacs-mu4e"
	     "mu"
	     "emacs-mu4e-alert"
	     "emacs-org-mime"
	     "emacs-projectile"
	     "emacs-counsel-projectile"
	     "emacs-yasnippet"
	     "emacs-magit"
	     "emacs-lsp-mode"
	     "emacs-lsp-ui"
	     "emacs-lsp-treemacs"
	     "emacs-geiser-guile"
	     "emacs-geiser"
	     "emacs-smartparens"
	     "emacs-company"
	     "emacs-company-box"
	     ;; "emacs-compile"
	     "emacs-vterm"

	     "emacs-password-store"
	     "emacs-exwm"
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
                        "/home/haider/dotfiles/guix-home/.bashrc"
                        "bashrc")))
              (bash-profile
                (list (local-file
                        "/home/haider/dotfiles/guix-home/.bash_profile"
                        "bash_profile"))))))))