;; THIS DOCUMENT IS MANAGED BY ORGMODE

(defun win/position-window-left-corner ()
  (interactive)
  (let* ((pos (frame-position))
	 (pos-x (car pos))
	 (pos-y (cdr pos)))

    (exwm-floating-move (- pos-x) (- pos-y))))

(defun win/position-window-right-corner ()
  (interactive)
  (let* ((pos (frame-position))
	 (pos-x (car pos))
	 (pos-y (cdr pos)))

    (exwm-floating-move (- (- 1366 (frame-pixel-width)) pos-x) (- pos-y))))

(defun exwm/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun exwm/exwm-update-title ()
  (pcase exwm-class-name
    ("qutebrowser" (exwm-workspace-rename-buffer (format "qutebrowser: %s" exwm-title)))
    ("mpv" (exwm-workspace-rename-buffer (format "%s" exwm-title)))))

(defun exwm/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun exwm/bind-function (key invocation &rest bindings)
  "Bind KEYs to FUNCTIONs globally"
  (while key
    (exwm-input-set-key (kbd key)
			`(lambda ()
			   (interactive)
			   (funcall ',invocation)))
    (setq key (pop bindings)
	  invocation (pop bindings))))

(defun exwm/bind-command (key command &rest bindings)
  "Bind KEYs to COMMANDs globally"
  (while key
    (exwm-input-set-key (kbd key)
			`(lambda ()
			   (interactive)
			   (exwm/run-in-background ,command)))
    (setq key (pop bindings)
	  command (pop bindings))))

(defun exwm/picom ()
  (interactive)
  (start-process-shell-command "picom" nil "picom"))

;; Hide The Mouse Cursor
(defun exwm/unclutter ()
  (interactive)
  (start-process-shell-command "unclutter" nil "unclutter -idle 0.01 -root"))

(defun exwm/kill-unclutter ()
  (interactive)
  (start-process-shell-command "kill unclutter" nil "pkill unclutter"))

(defun exwm/run-xmodmap ()
  (interactive)
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/.xmodmap"))

(defun exwm/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil  "feh --bg-scale ~/Wallpapers/main.png"))

(defun exwm/configure-desktop ()
  (interactive)
  (exwm/set-wallpaper)
  (exwm/picom)
  (start-process-shell-command "scroll speed" nil "xset r rate 200 50")
  (startup)
  (message "Post Initialization script...")
  (sleep-for 8)
  (start-process-shell-command "sound-effect" nil "mpv --no-video /home/haider/do-not-delete/startup.mp3"))

(defun exwm/exwm-init-hook ()
  (exwm-workspace-switch-create 1)
  (exwm/unclutter)
  (exwm/configure-desktop)
(exwm/run-in-background "dunst"))                      ;; Launch Dunst

(defun exwm/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("qutebrowser" (exwm-workspace-move-window 2))
    ("icecat" (exwm-workspace-move-window 2))
    ("blender" (exwm-workspace-move-window 6))
    ("Spinter" (exwm-floating-toggle-floating))
    ("mpv" (exwm-workspace-move-window 3))
    ("Alacritty" (exwm-layout-set-fullscreen))))

;; Hide the modeline on all floating windows
(add-hook 'exwm-floating-setup-hook
          (lambda ()
            (exwm-layout-hide-mode-line)))

(defun poly/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "Dashboard")
    (1 "Terminal")
    (2 "Web Browser")
    (3 "Videos")
    (4 "Documentation")
    (5 "Development")
    (6 "Blender")
    (7 "Chat")
    (8 "Mu4e")
    (9 "Mocp")))

(require 'desktop-environment)
(desktop-environment-volume-set "25%")

(setup (:pkg exwm)
  (setq exwm-workspace-number 9)
  (add-hook 'exwm-update-class-hook #'exwm/exwm-update-class)
  (add-hook 'exwm-update-title-hook #'exwm/exwm-update-title)
  (add-hook 'exwm-manage-finish-hook #'exwm/configure-window-by-class)
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 16)
  (exwm-systemtray-enable)
  (add-hook 'exwm-init-hook #'exwm/exwm-init-hook)

(setq exwm-input-prefix-keys
      '(?\C-x
        ?\C-u
        ?\C-h
        ?\M-x
        ?\M-`
        ?\M-&
        ?\s-q
        ?\s-f
        ?\M-:
        ?\C-\M-j  ;; Buffer list
        ?\C-\ ))  ;; Ctrl+Space

(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

(setq exwm-input-global-keys
      `(
        ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
        ([?\s-r] . exwm-reset)
        ([?\s-f] . exwm-layout-toggle-fullscreen)
        ([?\s-z] . exwm-layout-toggle-mode-line)
        ([?\s-b] . consult-buffer)
        ([?\s-g] . consult-buffer-other-frame)
        ([\f6] . desktop-environment-toggle-mute)
        ([\f7] . desktop-environment-volume-decrement)
        ([\f8] . desktop-environment-volume-increment)
        ([?\s-x] . exwm-floating-toggle-floating)
        ([?\s-j] . win/position-window-left-corner)
        ([?\s-k] . win/position-window-right-corner)
        ([?\s-m] . exwm/mpv-float)

        ;; Launch applications via shell command
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))

        ;; Switch workspace
        ([?\s-w] . exwm-workspace-switch)

        ([?\s-E] . (lambda () (interactive) (dired "~")))
        ([?\s-Q] . (lambda () (interactive) (kill-buffer)))

        ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

(exwm-enable))

(defun exwm/run-qute ()
  (interactive)
  (exwm/run-in-background "qutebrowser --qt-flag disable-seccomp-filter-sandbox")
  (start-process-shell-command "dunst" nil "dunstify \"Launching Qutebrowser...\"")
  (exwm-workspace-switch-create 2))

(defun exwm/run-icecat ()
  (exwm/run-in-background "icecat")
  (start-process-shell-command "dunst" nil "dunstify \"Launching Icecat...\"")
  (exwm-workspace-switch-create 2))

(defun exwm/run-alacritty ()
  (exwm/run-in-background "alacritty")
  (start-process-shell-command "dunst" nil "dunstify \"Launching Alacritty...\""))

(defun exwm/run-mocp ()
  (exwm/run-in-background "alacritty -e mocp")
  (start-process-shell-command "dunst" nil "dunstify \"Launching Mocp...\"")
  (exwm-workspace-switch-create 9))

(defun exwm/run-mpv ()
  (exwm/run-in-background "mpv")
  (start-process-shell-command "dunst" nil "dunstify \"Launching MPV...\"")
  (exwm-workspace-switch-create 3))

(defun exwm/mpv-float ()
  (interactive)
  (exwm-floating-toggle-floating)
  (exwm-layout-shrink-window 516)
  (exwm-layout-shrink-window-horizontally 960))

(defun exwm/run-blen ()
  (exwm/run-in-background "blender")
  (start-process-shell-command "dunst" nil "dunstify \"Launching Blender...\"")
  (exwm-workspace-switch-create 6))

(defun exwm/run-snip ()
  (exwm/run-in-background "flameshot")
  (start-process-shell-command "dunst" nil "dunstify \"Launching Flameshot...\""))

(defun exwm/run-slock ()
  (interactive)
  (start-process-shell-command "slock" nil "slock"))

(defun exwm/run-rofi ()
  (interactive)
  (start-process-shell-command "rofi" nil "rofi -show drun"))

(exwm/bind-function
 "s-SPC" 'exwm/run-rofi
 "M-s-b" 'exwm/run-qute
 "M-s-f" 'exwm/run-icecat
 "M-s-m" 'exwm/run-mocp
 "s-t" 'exwm/run-alacritty
 "M-s-v" 'exwm/run-mpv
 "C-s-b" 'exwm/run-blen
 "s-l" 'exwm/run-slock
 "s-s" 'exwm/run-snip
 "s-q" 'kill-buffer)

(defun startup ()
  (interactive)
  (exwm/run-xmodmap)
  (start-process-shell-command "dunst" nil "dunstify Progress: -h int:value:16")
  (exwm-workspace-switch-create 0)
  (start-process-shell-command "btop" nil "alacritty -e btop")
  (sleep-for 1)
  (start-process-shell-command "dunst" nil "dunstify Progress: -h int:value:32")
  (exwm-workspace-switch-create 1)
  (vterm)
  (start-process-shell-command "dunst" nil "dunstify Progress: -h int:value:50")
  (sleep-for 1)
  (exwm-workspace-switch-create 7)
  (elfeed)
  (elfeed-update)
  (sleep-for 1)
  (exwm-workspace-switch-create 8)
  (notmuch)
  (start-process-shell-command "dunst" nil "dunstify Progress: -h int:value:66")
  (sleep-for 1)
  (exwm-workspace-switch-create 9)
  (start-process-shell-command "pulsemixer" nil "alacritty -e pulsemixer")
  (start-process-shell-command "dunst" nil "dunstify Progress: -h int:value:82")
  (sleep-for 1)
  (exwm-workspace-switch-create 2)
  (exwm/run-qute)
  (start-process-shell-command "dunst" nil "dunstify Progress: -h int:value:100"))

(defun startup/password ()
  (interactive)
  (let ((password (read-passwd "ERC Password: ")))
    (exwm-workspace-switch-create 7)
    (chat/connect-irc password)))

(defun shutdown ()
  (interactive)
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password: "))
			 " | sudo -S shutdown")))

(defun reboot ()
  (interactive)
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password: "))
			 " | sudo -S reboot")))

(defvar poly/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun poly/kill-panel ()
  (interactive)
  (when poly/polybar-process
    (ignore-errors
      (kill-process poly/polybar-process)))
  (setq poly/polybar-process nil))

(defun poly/start-panel ()
  (interactive)
  (poly/kill-panel)
  (setq poly/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun poly/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun poly/send-polybar-exwm-workspace ()
  (poly/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'poly/send-polybar-exwm-workspace)

(defun poly/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "Dashboard")
    (1 "Terminal")
    (2 "Web Browser")
    (3 "Videos")
    (4 "Documentation")
    (5 "Development")
    (6 "Blender")
    (7 "Chat")
    (8 "Mu4e")
    (9 "Mocp")))
