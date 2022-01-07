(use-modules (gnu))
(use-modules (gnu packages emacs))
(use-modules (gnu packages emacs-xyz))
(use-modules (gnu packages xorg))
(use-modules (gnu packages certs))
(use-modules (gnu packages mail))
(use-modules (gnu packages vim))
(use-service-modules
  cups
  desktop
  networking
  ssh
  xorg)

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
                  (supplementary-groups
                   '("wheel"
		     "netdev"
		     "audio"
		     "video")))
                %base-user-accounts))

  (packages (append (list
                     ;; Window Manager
                     emacs emacs-exwm emacs-desktop-environment
		     ;; Emacs Packages
		     emacs-guix mu 
                     ;; terminal emulator
                     xterm
		     ;; Text Editor
		     vim
                     ;; for HTTPS access
                     nss-certs)
                    %base-packages))

  (services
    (append
      (list (service xfce-desktop-service-type)
            (service openssh-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services))

  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (targets (list "/dev/sda"))
      (keyboard-layout keyboard-layout)))

  (swap-devices
    (list (swap-space
            (target
              (uuid "fdc34943-1094-4a38-90c4-c07960e388ab")))))

  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "8abae596-c59e-418f-a7f0-e6a65ea54039"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
