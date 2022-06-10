(use-modules (gnu)
 (gnu packages certs)
 (gnu packages pulseaudio)
 (gnu packages shells)
 (gnu packages emacs))

(use-service-modules
 cups
 desktop
 networking
 ssh
 nix
 xorg
 virtualization)

(operating-system
 (locale "en_GB.utf8")
 (timezone "Europe/London")
 (keyboard-layout (keyboard-layout "gb" "extd"))
 (host-name "xps")
 (users (cons* (user-account
                (name "haider")
                (comment "Haider")
                (group "users")
                (home-directory "/home/haider")
                (shell (file-append zsh "/bin/zsh"))
                (supplementary-groups
                 '("wheel"
                   "netdev"
                   "audio"
                   "video"
                   "libvirt"
                   "kvm")))

               (user-account
                (name "hasan")
                (comment "Haider's Brother")
                (group "users")
                (home-directory "/home/hasan")
                (password "")
                (supplementary-groups
                 '("netdev"
                   "audio"
                   "video")))
               %base-user-accounts))

 (packages (append (map specification->package
                        '("nss-certs"
                          "binutils"
                          "blender"
                          "qutebrowser"
                          "ungoogled-chromium"
                          "mpv"
                          ;; "ccls"
                          "notmuch"
                          "msmtp"
                          "feh"
                          "wine64"
                          "l2md"
                          "dunst"
                          "unzip"
                          "clang"
                          ;; "bear"
                          "lxappearance"
                          "gnome-themes-standard"
                          "xset"

                          ;; Virt manager
                          "virt-manager"
                          "dconf" ;; Needed for virt-manager (See Guix note)
                          "adwaita-icon-theme"
                          "hicolor-icon-theme"
                          "qemu"

                          "git"
                          "openssh"
                          "node"
                          "flameshot"
                          "stow"
                          "isync"
                          "alacritty"
                          "pandoc"
                          "pinentry-tty"
                          "password-store"
                          "gnupg"
                          "alsa-utils"
                          "unclutter"
                          "xmodmap"
                          "pulsemixer"
                          "font-jetbrains-mono"
                          "libvterm"
                          "picom"
                          "flatpak"

                          ;; Emacs Packages
                          "vim"
                          "emacs"
                          "ispell"
                          "emacs-guix"
                          "emacs-gnus-alias"
                          "emacs-general"
                          "emacs-elfeed"

                          "emacs-ytel"
                          "emacs-ytel-show"

                          "emacs-notmuch"
                          "emacs-undo-tree"
                          "emacs-evil"
                          "emacs-evil-collection"
                          "emacs-doom-themes"
                          "emacs-visual-fill-column"
                          "emacs-vertico"
                          "emacs-marginalia"
                          "emacs-rainbow-delimiters"
                          "emacs-all-the-icons"
                          "emacs-all-the-icons-completion"
                          "emacs-doom-modeline"
                          "emacs-which-key"
                          "emacs-diminish"
                          "emacs-counsel"
                          ;; "emacs-helpful"
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
                          "gcc"
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
                          ;; "emacs-ccls"
                          "emacs-lsp-mode"
                          "emacs-lsp-ui"
                          "emacs-lsp-treemacs"
                          "emacs-geiser-guile"
                          "emacs-geiser"
                          "emacs-smartparens"
                          "emacs-company"
                          "emacs-company-box"
                          "emacs-vterm"
                          "emacs-password-store"
                          "emacs-exwm"
                          "emacs-desktop-environment"
                          "emacs-consult"))
                   %base-packages))

 (services
  (append
   (list
    (service slim-service-type (slim-configuration
                                (xorg-configuration
                                 (xorg-configuration
                                  (keyboard-layout keyboard-layout)))))

    (service libvirt-service-type
             (libvirt-configuration
              (unix-sock-group "libvirt")
              (tls-port "16555")))

    (service nix-service-type)

    (extra-special-file
     "/lib64/ld-linux-x86-64.so.2"
     (file-append glibc "/lib/ld-linux-x86-64.so.2"))

    (extra-special-file
     "/usr/bin/env"
     (file-append coreutils "/bin/env")))

   (modify-services %desktop-services
                    (delete gdm-service-type))))

 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets (list "/dev/sda"))
   (keyboard-layout keyboard-layout)))

 (swap-devices
  (list (swap-space
         (target
          (uuid "c12505e5-3ecd-4ca3-a71a-ee01bbe9116b")))))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "adf17b8c-6726-4048-8671-5ba77da3f58d"
                 'ext4))
          (type "ext4"))

         (file-system
          (device (uuid "83f6e0ed-4743-4f32-94be-f8ffe2f029c6"))
          (mount-point "/home/haider/storage/")
          (type "ext4"))
         %base-file-systems)))
