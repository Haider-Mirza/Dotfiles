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
         (list "pipewire"
               "blender"
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
               "emacs"
               "unclutter"
               "xmodmap"
               "alsa-utils"
               "font-jetbrains-mono"
               "libvterm"
               "slock")))
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
