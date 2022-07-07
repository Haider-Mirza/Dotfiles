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
 sddm
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
                        '("xmonad-next"
			  "xmessage"
			  "ghc"
			  "ghc-xmonad-contrib-next"
			  "xinitrc-xsession"
			  "xinit"
			  "xmobar"
			  "dmenu"
			  "nss-certs"
                          "binutils"))
                   %base-packages))

 (services
  (append
   (list
    (service sddm-service-type 
	     (sddm-configuration
	      (auto-login-user "haider")
	      (auto-login-session "xinitrc")))
    (set-xorg-configuration
      (xorg-configuration
       (keyboard-layout keyboard-layout))
     sddm-service-type)

    (service libvirt-service-type
             (libvirt-configuration
              (unix-sock-group "libvirt")
              (tls-port "16555")))

    ;; (service nix-service-type)

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
