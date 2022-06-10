;; THIS DOCUMENT IS MANAGED BY ORGMODE

(setq dw/is-guix-system (and (eq system-type 'gnu/linux)
                             (require 'f)
                             (string-equal (f-read "/etc/issue")
                                           "\nThis is the GNU system.  Welcome.\n")))

(unless (featurep 'straight)
  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
(require 'setup)

;; Uncomment this for debugging purposes
;; (defun dw/log-require (&rest args)
;;   (with-current-buffer (get-buffer-create "*require-log*")
;;     (insert (format "%s\n"
;;                     (file-name-nondirectory (car args))))))
;; (add-to-list 'after-load-functions #'dw/log-require)

;; Recipe is always a list
;; Install via Guix if length == 1 or :guix t is present

(defvar dw/guix-emacs-packages '()
  "Contains a list of all Emacs package names that must be
installed via Guix.")

;; Examples:
;; - (org-roam :straight t)
;; - (git-gutter :straight git-gutter-fringe)

(defun dw/filter-straight-recipe (recipe)
  (let* ((plist (cdr recipe))
         (name (plist-get plist :straight)))
    (cons (if (and name (not (equal name t)))
              name
            (car recipe))
          (plist-put plist :straight nil))))

(setup-define :pkg
  (lambda (&rest recipe)
    (if (and dw/is-guix-system
             (or (eq (length recipe) 1)
                 (plist-get (cdr recipe) :guix)))
        `(add-to-list 'dw/guix-emacs-packages
                      ,(or (plist-get recipe :guix)
                           (concat "emacs-" (symbol-name (car recipe)))))
      `(straight-use-package ',(dw/filter-straight-recipe recipe))))
  :documentation "Install RECIPE via Guix or straight.el"
  :shorthand #'cadr)

(setup-define :delay
  (lambda (&rest time)
    `(run-with-idle-timer ,(or time 1)
                          nil ;; Don't repeat
                          (lambda () (require ',(setup-get 'feature)))))
  :documentation "Delay loading the feature until a certain amount of idle time has passed.")

(setup-define :disabled
  (lambda ()
    `,(setup-quit))
  :documentation "Always stop evaluating the body.")

(setup-define :load-after
  (lambda (features &rest body)
    (let ((body `(progn
                   (require ',(setup-get 'feature))
                   ,@body)))
      (dolist (feature (if (listp features)
                           (nreverse features)
                         (list features)))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES."
  :indent 1)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setup (:pkg general)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "SPC" '(find-file :which-key "find file")))

(setup (:pkg undo-tree)
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

(setup (:pkg evil)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)

  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(evil-mode 1)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setup (:pkg evil-collection)
  (evil-collection-init))

(setq initial-scratch-message "Make sure to check OrgAgenda and OrgRoam Dailies!\nRun: (exwm/startup)")

(set-face-attribute 'default nil
                    :family "Jetbrains Mono"
                    :height 80
                    :weight 'normal
                    :width  'normal)

(setup (:pkg doom-themes))
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-molokai t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; Disable this anoyying visible bell
(setq visible-bell nil)

;; (defun org/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 180
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (setup (:pkg visual-fill-column)
;;   (:hook-into org-mode))

(setup (:pkg beacon))

(setup (:pkg vertico)
  (vertico-mode)
  (:with-map vertico-map
    (:bind "C-j" vertico-next
           "C-k" vertico-previous
           "C-f" vertico-exit))
  (:option vertico-cycle t))

(setup savehist
  (savehist-mode 1))

(setup (:pkg marginalia)
  (:option marginalia-annotators '(marginalia-annotators-heavy
                                   marginalia-annotators-light
                                   nil))
  (setq marginalia-align 'right)
  (marginalia-mode))

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(setup (:pkg rainbow-delimiters)
  (:hook-into prog-mode))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(;; org-mode-hook
                term-mode-hook
                dashboard-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setup (:pkg all-the-icons))

(setup (:pkg all-the-icons-completion)
  (all-the-icons-completion-mode))

(setup (:pkg doom-modeline)
  (:hook-into after-init-hook)
  (:option doom-modeline-lsp t
           doom-modeline-height 10
           doom-modeline-buffer-encoding nil
           doom-modeline-github nil
           doom-modeline-project-detection 'auto
           doom-modeline-number-limit 99
           doom-modeline-irc t)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  ;; Enable the time & date in the modeline
  (setq display-time-string-forms '((format-time-string "%H:%M:%S" now)))
  (setq display-time-interval 1)
  (display-time-mode 1))

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(setup (:pkg which-key)
  (diminish 'which-key-mode)
  (which-key-mode)
  (setq which-key-idle-delay 1))

(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

(rune/leader-keys
  "x"  '(:ignore t :which-key "Delete")
  "c"  '(:ignore t :which-key "Create")
  "xf" '(delete-file :which-key "Delete file")
  "xd" '(delete-directory :which-key "Delete directory")
  "cf" '(make-empty-file :which-key "Create empty file")
  "cf" '(make-directory :which-key "Create directory"))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(run-at-time nil (* 5 60) 'recentf-save-list)

(rune/leader-keys
  "t" '(counsel-recentf :which-key "Recent files"))

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)

(setup (:pkg counsel)
  (:bind "M-x" counsel-M-x))

;; (setup (:pkg helpful))
;; I personally never use it (but I should)

(setup (:pkg hydra)
  (require 'hydra))

;; This needs a more elegant ASCII banner
(defhydra hydra-exwm-move-resize
  (global-map "<C-M-tab>")
  "Move/Resize Window (Shift is bigger steps, Ctrl moves window)"
  ("j" (lambda () (interactive) (exwm-layout-enlarge-window 10)) "V 10")
  ("J" (lambda () (interactive) (exwm-layout-enlarge-window 30)) "V 30")
  ("k" (lambda () (interactive) (exwm-layout-shrink-window 10)) "^ 10")
  ("K" (lambda () (interactive) (exwm-layout-shrink-window 30)) "^ 30")
  ("h" (lambda () (interactive) (exwm-layout-shrink-window-horizontally 10)) "< 10")
  ("H" (lambda () (interactive) (exwm-layout-shrink-window-horizontally 30)) "< 30")
  ("l" (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 10)) "> 10")
  ("L" (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 30)) "> 30")
  ("C-j" (lambda () (interactive) (exwm-floating-move 0 10)) "V 10")
  ("C-S-j" (lambda () (interactive) (exwm-floating-move 0 30)) "V 30")
  ("C-k" (lambda () (interactive) (exwm-floating-move 0 -10)) "^ 10")
  ("C-S-k" (lambda () (interactive) (exwm-floating-move 0 -30)) "^ 30")
  ("C-h" (lambda () (interactive) (exwm-floating-move -10 0)) "< 10")
  ("C-S-h" (lambda () (interactive) (exwm-floating-move -30 0)) "< 30")
  ("C-l" (lambda () (interactive) (exwm-floating-move 10 0)) "> 10")
  ("C-S-l" (lambda () (interactive) (exwm-floating-move 30 0)) "> 30")
  ("f" nil "finished" :exit t))

(setup (:pkg consult))

(setq mpv-playlist.txt "~/.config/qutebrowser/playlist.txt")

(defun mpv-playlist-add ()
  "Insert web videos to a playlist.txt"
  (interactive)
  (setq mpv-query (concat (read-string "Information: ") "-" (read-string "Paste URL: ")))
  (start-process-shell-command "to-file" nil (concat "printf \"" mpv-query "\n\">> " mpv-playlist.txt)))


(defun mpv-playlist-load ()
  "Load web videos from playlist.txt"
  (interactive)
  (setq mpv-playlist-line
        (completing-read "Select Video: "
                         (with-current-buffer (find-file-noselect mpv-playlist.txt)
                           (mapcar (lambda (x) (split-string x " " t))
                                   (split-string
                                    (buffer-substring-no-properties (point-min) (point-max))
                                    "\n")))))


  (setq mpv-selected-video (delete (car (split-string mpv-playlist-line "-")) (split-string mpv-playlist-line "-")))

  (start-process-shell-command "launch mpv" nil (mapconcat 'identity (append '("mpv") mpv-selected-video) " "))
  (exwm-workspace-switch-create 3))

(rune/leader-keys
  "v"  '(:ignore t :which-key "Video")
  "va" '(mpv-playlist-add :which-key "Add a video to my mpv playlist")
  "vl" '(mpv-playlist-load :which-key "Load a video from my mpv playlist"))

(setup (:pkg no-littering)
  (require 'no-littering))

(setq backup-by-copying t)

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(defun my-save-word ()
  "Save a word to a dictionary that is stored in ~/.aspell.en.pws"
  (interactive)
  (let ((current-location (point))
	(word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(rune/leader-keys
  "f"  '(:ignore t :which-key "Flyspell")
  "fm" '(flyspell-mode :which-key "Start Flyspell-Mode")
  "fa" '(my-save-word :which-key "Add word to Flyspell"))

(setq-default abbrev-mode t) ;; Enable abbrev-mode

  (read-abbrev-file "~/.emacs.d/abbrev_defs")

(setq save-abbrevs 'silent)

  (setq abbrev-file-name
        "~/.emacs.d/abbrev_defs")

  (rune/leader-keys
    "s"  '(:ignore t :which-key "Abbrev")
    "sa" '(add-global-abbrev :which-key "Add word to abbrev globally"))

(global-set-key (kbd "C-s-s") 'swiper)

(rune/leader-keys
  "TAB" '(comment-dwim :which-key "comment lines"))

(setup (:pkg prettier))

(rune/leader-keys
  "o"  '(:ignore t :which-key "Org")
  "oa" '(org-agenda :which-key "View Org-Agenda")
  "ol" '(org-agenda-list :which-key "View Org-Agendalist")
  "oL" '(org-insert-link :which-key "View Org-Agendalist")
  "ot" '(org-babel-tangle :which-key "Tangle Document")
  "ox" '(org-export-dispatch :which-key "Export Document")
  "od" '(org-deadline :which-key "Deadline")
  "os" '(org-schedule :which-key "Schedule")
  "oh" '(org-todo :which-key "Add a todo header thing"))

(setup (:pkg org)
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done t)
  (setq org-log-into-drawer t)

  (setq org-src-fontify-natively t) ;; Syntax highlighting in org src blocks
  (setq org-startup-folded t) ;; Org files start up folded by default
  (setq org-image-actual-width nil)

(add-hook 'org-mode-hook (lambda ()(org-toggle-pretty-entities)(org-make-toc-mode)(flyspell-mode)))

  (setq org-agenda-files
        '("~/documents/Home/Reminders.org"
          "~/documents/Home/TODO.org"
          "~/documents/School/Homework.org"
          "~/documents/School/School-Reminders.org"))

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "WORK(w)"
           "EVENT(e)"
           "RESEARCH(r)"
           "HOLD(h)"
           "PLAN(p)"
           "|"
           "DONE(d)"
           "FAILED(f)")))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-agenda-span 'month))

(setup (:pkg org-superstar)
  (:hook-into org-mode))

(setq org-startup-indented t)           ;; Indent according to section
(setq org-startup-with-inline-images t) ;; Display images in-buffer by default

(setup (:pkg pandoc))

(setup (:pkg org-appear)
  (:hook-into org-mode)
  (setq org-hide-emphasis-markers t) ;; A default setting that needs to be t for org-appear
  (setq org-appear-autoemphasis t)  ;; Enable org-appear on emphasis (bold, italics, etc)
  (setq org-appear-autolinks t) ;; Enable on links
  (setq org-appear-autosubmarkers t)) ;; Enable on subscript and superscript

;; (setup (:pkg org-super-agenda)
;;   (setq org-agenda-skip-scheduled-if-done t
;;         org-agenda-skip-deadline-if-done t
;;         org-agenda-include-deadlines t
;;         org-agenda-include-diary t
;;         org-agenda-block-separator nil
;;         org-agenda-compact-blocks t
;;         org-agenda-start-with-log-mode t)

;;   (setq org-agenda-span 'day)
;;   (setq org-super-agenda-groups
;;         '((:name "Important"
;;                  :priority "a")
;;           (:name "Due today"
;;                  :deadline today)
;;           (:name "Overdue"
;;                  :deadline past)
;;           (:name "Things todo"
;;                  :todo "TODO")
;;           (:name "School work"
;;                  :todo "WORK")
;;           (:name "Completed"
;;                  :todo "COMPLETED")))
;;   (org-super-agenda-mode 1))

;; (setup (:pkg evil-org)
;;   ;; (:hook-into org-mode org-agenda-mode)
;;   (require 'evil-org)
;;   (require 'evil-org-agenda)
;;   (evil-org-set-key-theme '(navigation todo insert textobjects additional))
;;   (evil-org-agenda-seemacs-orgt-keys))

(setup (:pkg org-roam-ui :straight t))
(setup (:pkg org-roam)
  (setq org-roam-v2-ack t)
  (setq org-roam-db-location (concat (getenv "HOME") "/Notes/org-roam.db"))
  (setq dw/daily-note-filename "%<%Y-%m-%d>.org"
        dw/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")

  (:when-loaded
    (org-roam-db-autosync-mode))

  (:option
   org-roam-directory "~/Notes/"
   org-roam-dailies-directory "Journal/"
   org-roam-completion-everywhere t
   org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ("s" "school" plain "\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#filetags: School")
      :unnarrowed t))
   org-roam-dailies-capture-templates
   `(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: DailyDef"))
     ("t" "todo" entry "* TODO: \n%?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: DailyTodo"))
     ("d" "diary" entry "* Diary: \n%?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: DailyDiary")))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(rune/leader-keys
  "or"  '(:ignore t :which-key "Org-Roam")
  "orc" '(org-roam-capture :which-key "Capture a node")
  "ori" '(org-roam-node-insert :which-key "Insert note")
  "orI" '(org-roam-node-insert-immediate :which-key "Insert and create a new node without opening it")
  "orf" '(org-roam-node-find :which-key "Find a node")
  "ort" '(org-roam-buffer-toggle :which-key "Toggle")

  "w"  '(:ignore t :which-key "Dailies")
  "wct" '(org-roam-dailies-capture-today :which-key "Capture daily for Today")
  "wcy" '(org-roam-dailies-capture-yesterday :which-key "Capture daily for Yesterday")
  "wcT" '(org-roam-dailies-capture-tomorrow :which-key "Capture daily for Tomorrow")
  "wcd" '(org-roam-dailies-capture-date :which-key "Capture daily for certain date")
  "wgt" '(org-roam-dailies-goto-today :which-key "Check Today's daily")
  "wgy" '(org-roam-dailies-goto-yesterday :which-key "Check Yesterday's daily")
  "wgT" '(org-roam-dailies-goto-tomorrow :which-key "Check Tommorow's daily")
  "wgd" '(org-roam-dailies-goto-date :which-key "Check daily for a specific date"))

(setup (:pkg org-auto-tangle)
  ;; (require 'org-auto-tangle)
  ;; (add-hook 'org-mode-hook 'org-auto-tangle-mode)
  )

(require 'erc) ;; Notifications require this to be required

(setq erc-server "irc.libera.chat"
      erc-nick "Haider"
      erc-user-full-name "Haider Mirza"
      erc-rename-buffers t
      erc-track-shorten-start 8
      ;; erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs" "#guix"))
      erc-kill-buffer-on-part t
      ;; erc-fill-column 120
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20
      erc-auto-query 'bury
      erc-track-exclude '("#emacs")
      ;; erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY")
      ;; erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY")
      erc-track-exclude-server-buffer t
      erc-track-enable-keybindings t
      erc-quit-reason (lambda (s) (or s "Ejected from the cyberspace!"))
      erc-track-visibility nil) ;; Essential if using EXWM


(defun chat/connect-irc (password)
  (interactive)
  (erc-tls
   :server "irc.libera.chat"
   :port 6697
   :nick "Haider"
   :password password))

(setup (:pkg erc-hl-nicks)
  (add-to-list 'erc-modules 'hl-nicks))

(setup (:pkg erc-image)
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image))

(add-to-list 'erc-modules 'notifications)

(rune/leader-keys
  "i"  '(:ignore t :which-key "IRC")
  "ii" '(chat/connect-irc :which-key "launch IRC")
  "ib" '(erc-switch-to-buffer :which-key "Switch Buffer"))

(setup (:pkg elfeed-goodies :straight t)
  (setq elfeed-goodies/entry-pane-size 0.5)
  (elfeed-goodies/setup))

(setup (:pkg elfeed)
  (setq elfeed-feeds
        '(("https://www.archlinux.org/feeds/news/" linux)
          ("https://www.reddit.com/r/emacs.rss" reddit linux)
          ("https://www.reddit.com/r/linux.rss" reddit linux)
          ("https://sachachua.com/blog/feed/" linux emacs))))

(setup (:pkg emojify)
  (add-hook 'after-init-hook #'global-emojify-mode))

(rune/leader-keys
  "a"  '(:ignore t :which-key "Emojify") ;; I know a has no correlation but Im running out of space ok.
  "ai" '(emojify-insert-emoji :which-key "Insert Emoji"))

(setup (:pkg unicode-fonts))

(rune/leader-keys
  "d"  '(:ignore t :which-key "Files")
  "dt" '((lambda() (interactive) (find-file "~/documents/Home/TODO.org")) :which-key "TODO")
  "ds" '((lambda() (interactive) (find-file "~/documents/Home/Reminders.org")) :which-key "Schedule")
  "dh" '((lambda() (interactive) (find-file "~/documents/School/Homework.org")) :which-key "Homework")
  "dr" '((lambda() (interactive) (find-file "~/documents/School/School-Reminders.org")) :which-key "Reminders"))

(rune/leader-keys
  "c"  '(:ignore t :which-key "Files")
  "ce" '((lambda() (interactive) (find-file "~/dotfiles/emacs.org")) :which-key "Emacs config")
  "cd" '((lambda() (interactive) (find-file "~/dotfiles/desktop.org")) :which-key "Desktop config")
  "cs" '((lambda() (interactive) (find-file "~/dotfiles/system.org")) :which-key "System config")
  "cp" '((lambda() (interactive) (find-file "~/dotfiles/programs.org")) :which-key "Programs config"))

;; TODO: Switch to the Guix package
(setup (:pkg gnus-alias :straight t))
(setup (:pkg notmuch :straight t)
  (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
  (setq message-kill-buffer-on-exit t
        notmuch-search-oldest-first 'nil
        mml-default-sign-method 'gpg
        mml-secure-openpgp-signers '("14C4247F03ADBB49D74739209BB51EC33CB46308")
        notmuch-fcc-dirs
        '(("haider@haider.gq" .
           "\"haider@haider.gq/sent\" +sent +work -inbox -unread")
          (".*" . "\"ha6mi19@keaston.bham.sch.uk/sent\" +sent +school -inbox -unread"))
        notmuch-saved-searches
        '((:name "Unread" :query "tag:unread" :key "u")
          (:name "Inbox" :query "tag:inbox" :key "i")
          (:name "Guix" :query "tag:guix" :key "g")
          (:name "Mailing Lists" :query "tag:list" :key "m")
          (:name "Sent" :query "tag:sent" :key "s")
          (:name "Spam" :query "tag:spam" :key "S")
          (:name "Drafts" :query "tag:draft" :key "d")
          (:name "Trash" :query "tag:trash" :key "t")
          (:name "Archive" :query "tag:archive" :key "a")
          (:name "All mail" :query "*" :key "A")))

  ;; Modeline
  (setq modeline/notmuch-activity-string "")
  (add-to-list 'global-mode-string '((:eval modeline/notmuch-activity-string)) t)

  (defun modeline/get-notmuch-incoming-count ()
    (string-trim
     (shell-command-to-string
      "notmuch count tag:unread")))

  (defun modeline/format-notmuch-mode-string (count)
    (let* ((no-email (string= count "0"))
           (email-text (if no-email " | No Mail" " | Unread Emails")))
      (concat email-text " [" (if no-email "" count) "] |")))

  (defun modeline/update-notmuch-activity-string (&rest args)
    (interactive)
    (setq modeline/notmuch-activity-string
          (modeline/format-notmuch-mode-string (modeline/get-notmuch-incoming-count)))
    (force-mode-line-update))

  (add-hook 'notmuch-after-tag-hook 'modeline/update-notmuch-activity-string)
  (add-hook 'after-init-hook #'modeline/update-notmuch-activity-string)

  ;; Update Notmuch
  (defun notmuch/update ()
    "Update Emails and load them into Notmuch's database "
    (interactive)
    (start-process-shell-command "Update Emails" nil "chmod +x ~/mail/.notmuch/hooks/pre-new ~/mail/.notmuch/hooks/post-new && notmuch new"))

  ;; Manual tagging
  (defun notmuch/trash()
    "Set the current message to trash"
    (interactive)
    (evil-mode 0)
    (if (string= (buffer-local-value 'major-mode (current-buffer)) "notmuch-search-mode")
        (notmuch-search-tag '("+trash" "-inbox" "-archived" "-unread"))
      (if (string= (buffer-local-value 'major-mode (current-buffer)) "notmuch-show-mode")
          (notmuch-show-tag '("+trash" "-inbox" "-archived" "-unread"))))
    (evil-mode 1))

  (defun notmuch/read()
    "Set the current message to read"
    (interactive)
    (if (string= (buffer-local-value 'major-mode (current-buffer)) "notmuch-search-mode")
        (notmuch-search-tag '("-unread"))
      (if (string= (buffer-local-value 'major-mode (current-buffer)) "notmuch-show-mode")
          (notmuch-show-tag '("-unread")))))

  (defun notmuch/archive()
    "Archive a message"
    (interactive)
    (if (string= (buffer-local-value 'major-mode (current-buffer)) "notmuch-search-mode")
        (notmuch-search-tag '("+archive"))
      (if (string= (buffer-local-value 'major-mode (current-buffer)) "notmuch-show-mode")
          (notmuch-show-tag '("+archive")))))

  (add-hook 'message-setup-hook
            (lambda ()
              (gnus-alias-determine-identity)
              (define-key message-mode-map (kbd "C-c f")
                (lambda ()
                  (interactive)
                  (message-remove-header "Fcc")
                  (message-remove-header "Organization")
                  (gnus-alias-select-identity)
                  (notmuch-fcc-header-setup)))
              (flyspell-mode)))

                                        ; gnus-alias
  (autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
  (setq gnus-alias-identity-alist
        '(("work"
           nil ;; Does not refer to any other identity
           "Haider Mirza <haider@haider.gq>"
           nil ;; No organization header
           nil ;; No extra headers
           nil ;; No extra body text
           nil)
          ("school"
           nil ;; Does not refer to any other identity
           "Haider Mirza <ha6mi19@keaston.bham.sch.uk>"
           nil 
           nil ;; No extra headers
           nil ;; No extra body text
           nil)))

  (setq gnus-alias-default-identity "work")

  ;; Message sending hooks
  ;; (add-hook 'message-send-hook
  ;; 	    (lambda ()
  ;; 	      (let ((answer (read-from-minibuffer "Sign or encrypt?\nEmpty to do nothing.\n[s/e]: ")))
  ;; 		(cond
  ;; 		 ((string-equal answer "s") (progn
  ;; 					      (message "Signing message.")
  ;; 					      (mml-secure-message-sign-pgpmime)))
  ;; 		 ((string-equal answer "e") (progn
  ;; 					      (message "Encrypt and signing message.")
  ;; 					      (mml-secure-message-encrypt-pgpmime)))
  ;; 		 (t (progn
  ;; 		      (message "Dont signing or encrypting message.")
  ;; 		      nil))))))

  (setq send-mail-function 'sendmail-send-it
        sendmail-program (executable-find "msmtp")
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header))

(rune/leader-keys
  "n"  '(:ignore t :which-key "Notmuch")
  "nu" '(notmuch/update :which-key "Notmuch Update")
  "nh" '(notmuch-hello-update :which-key "Notmuch Update Hello buffer")
  "nt" '(notmuch/trash :which-key "Notmuch Trash")
  "na" '(notmuch/archive :which-key "Notmuch Archive")
  "nr" '(notmuch/read :which-key "Notmuch Read"))

;; (setup mu4e

;;   (require 'mu4e-org)

;;   ;; This is set to 't' to avoid mail syncing issues when using mbsync
;;   (setq mu4e-change-filenames-when-moving t)

;;   (setq org-capture-templates
;; 	`(("m" "Email Workflow")
;; 	  ("mf" "Follow Up" entry (file+headline "~/org/Mail.org" "Follow Up")
;; 	   "* TODO %a\n\n  %i")
;; 	  ("mr" "Read Later" entry (file+headline "~/org/Mail.org" "Read Later")
;; 	   "* TODO %a\n\n  %i")))

;;   ;; Refresh mail using isync every 10 minutes
;;   (setq mu4e-update-interval (* 10 60))
;;   (setq mu4e-get-mail-command "mbsync -a")
;;   (setq mu4e-maildir "~/Mail")

;; 	  :enter-func (lambda ()
;; 			(mu4e-message "Entering personal context")
;; 			(when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
;; 			  (revert-buffer)))
;; 	  :leave-func (lambda ()
;; 			(mu4e-message "Leaving personal context")
;; 			(when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
;; 			  (revert-buffer)))
;; 	  :match-func
;; 	  (lambda (msg)
;; 	    (when msg
;; 	      (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
;; 	  :vars '((user-mail-address . "haider@haider.gq")
;; 		  (user-full-name    . "Haider Mirza")
;; 		  ;; (mu4e-compose-signature . "Haider Mirza via Emacs on a GNU/Linux system")
;; 		  (smtpmail-smtp-server  . "smtp.gmail.com")
;; 		  (smtpmail-smtp-service . 465)
;; 		  (smtpmail-stream-type  . ssl)
;; 		  (mu4e-drafts-folder  . "/Gmail/[Gmail]/Drafts")
;; 		  (mu4e-sent-folder  . "/Gmail/[Gmail]/Sent Mail")
;; 		  (mu4e-refile-folder  . "/Gmail/[Gmail]/All Mail")
;; 		  (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")))

;; 	 ;; Work account
;; 	 (make-mu4e-context
;; 	  :name "School"
;; 	  :enter-func (lambda ()
;; 			(mu4e-message "Entering school context")
;; 			(when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
;; 			  (revert-buffer)))
;; 	  :leave-func (lambda ()
;; 			(mu4e-message "Leaving school context")
;; 			(when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
;; 			  (revert-buffer)))
;; 	  :match-func
;; 	  (lambda (msg)
;; 	    (when msg
;; 	      (string-prefix-p "/Outlook" (mu4e-message-field msg :maildir))))
;; 	  :vars '((user-mail-address . "ha6mi19@keaston.bham.sch.uk")
;; 		  (user-full-name    . "Haider Mirza")
;; 		  ;;(mu4e-compose-signature . "Haider Mirza via Emacs on a GNU/Linux system")
;; 		  (mu4e-compose-signature . nil) ;; Mu4e signature comes out to be another seperate file.
;; 		  ;; (smtpmail-smtp-server  . "smtp-mail.outlook.com")
;; 		  ;; (smtpmail-smtp-service . 587)
;; 		  ;; (smtpmail-stream-type  . ssl)
;; 		  (mu4e-drafts-folder  . "/Outlook/Drafts")
;; 		  (mu4e-sent-folder  . "/Outlook/Sent")
;; 		  (mu4e-refile-folder  . "/Outlook/Archive")
;; 		  (mu4e-trash-folder  . "/Outlook/Trash")))))

;;   (add-to-list 'mu4e-bookmarks '("m:/Outlook/INBOX or m:/Gmail/Inbox" "All Inboxes" ?i))

;;   ;; ;; Sign all of my emails with opengpg keys 
;;   ;; (setq mml-secure-openpgp-signers '("2C52DB235E0FB36C"))
;;   ;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

;;   ;; (setq mu4e-context-policy 'pick-first)

;;   (defun sign-or-encrypt-message ()
;;     (let ((answer (read-from-minibuffer "Sign or encrypt?\nEmpty to do nothing.\n[s/e]: ")))
;;       (cond
;;        ((string-equal answer "s") (progn
;; 				    (message "Signing message.")
;; 				    (mml-secure-message-sign-pgpmime)))
;;        ((string-equal answer "e") (progn
;; 				    (message "Encrypt and signing message.")
;; 				    (mml-secure-message-encrypt-pgpmime)))
;;        (t (progn
;; 	    (message "Dont signing or encrypting message.")
;; 	    nil)))))

;;   (add-hook 'message-send-hook 'sign-or-encrypt-message)

;;   (setq mu4e-maildir-shortcuts
;; 	'((:maildir "/Gmail/Inbox"    :key ?g)
;; 	  (:maildir "/Outlook/INBOX"     :key ?i)
;; 	  (:maildir "/Gmail/[Gmail]/Sent Mail" :key ?s)
;; 	  (:maildir "/Outlook/Sent" :key ?S))))

;; ;; Make sure plain text mails flow correctly for recipients
;; (setq mu4e-compose-format-flowed t)

;; (setup (:pkg mu4e-alert)
;;   (mu4e-alert-enable-mode-line-display)
;;   (mu4e-alert-set-default-style 'libnotify)
;;   (:hook-into after-init-hook mu4e-alert-enable-notifications))

;; (setup (:pkg org-mime)
;;   (setq org-mime-export-options '(:section-numbers nil
;; 						   :with-author nil
;; 						   :with-toc nil)))

;; (add-hook 'org-mime-html-hook
;; 	  (lambda ()
;; 	    (org-mime-change-element-style
;; 	     "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
;; 			   "#E6E1DC" "#232323"))))

;; (add-hook 'message-send-hook 'org-mime-htmlize)

(setup (:pkg projectile)
  (:with-map
      (:bind projectile-command-map))
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-find-file))

(setup (:pkg counsel-projectile)
  (counsel-projectile-mode))

(setup (:pkg yasnippet)
  (setq yas-snippet-dirs '(
                           "~/.emacs.d/etc/yasnippet/snippets"
                           "~/guix/etc/snippets"))
  (yas-global-mode))

(setup (:pkg magit))
;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(rune/leader-keys
  "m"  '(:ignore t :which-key "Magit")
  "ms" '(magit-status :which-key "Magit Status"))

(setup (:pkg lsp-mode)
  (:bind "TAB" completion-at-point)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)

  (setq gc-cons-threshold (* 100 1024 1024)
        lsp-clangd-binary-path "/run/current-system/profile/bin/clangd"
        ;; lsp-diagnostics-provider :none
        ;; lsp-modeline-diagnostics-enable nil
        ;; lsp-headerline-breadcrumb-enable-diagnostics nil
        read-process-output-max (* 1024 1024)
        ;; treemacs-space-between-root-nodes nil
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 0.0))

;; (defun dw/set-js-indentation ()
;;   (setq js-indent-level 2)
;;   (setq evil-shift-width js-indent-level)
;;   (setq-default tab-width 2))

;; (setup (:pkg js2-mode
;;   :mode "\\.jsx?\\'"
;;   :config
;;   ;; Use js2-mode for Node scripts
;;   (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

;;   ;; Don't use built-in syntax checking
;;   (setq js2-mode-show-strict-warnings nil)

;;   ;; Set up proper indentation in JavaScript and JSON files
;;   (add-hook 'js2-mode-hook #'dw/set-js-indentation)
;;   (add-hook 'json-mode-hook #'dw/set-js-indentation))


;; (setup (:pkg apheleia
;;   :defer 10
;;   :config
;;   (apheleia-global-mode +1))

;; (setup (:pkg prettier-js
;;   :defer 10
;;   ;; :hook ((js2-mode . prettier-js-mode)
;;   ;;        (typescript-mode . prettier-js-mode))
;;   :config
;;   (setq prettier-js-show-errors nil))

(setup (:pkg cmake-mode :straight t))

(rune/leader-keys
  "e"  '(:ignore t :which-key "E-Lisp")
  "el" '(eval-last-sexp :which-key "Evaluate last sexpression")
  "er" '(eval-region :which-key "Evaluate elisp in region"))

(setup (:pkg ccls :straight t))

;; (setup (:pkg rustic
;;   :ensure
;;   :config
;;   ;; comment to disable rustfmt on save
;;   (setq rustic-format-on-save t)
;;   (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; (defun rk/rustic-mode-hook ()
;;   ;; so that run C-c C-c C-r works without having to confirm, but don't try to
;;   ;; save rust buffers that are not file visiting. Once
;;   ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
;;   ;; no longer be necessary.
;;   (when buffer-file-name
;;     (setq-local buffer-save-without-query t)))

;; (setup (:pkg rust-playground :ensure)

;; (setup (:pkg toml-mode :ensure)

;; (rune/leader-keys
;;   "r"  '(:ignore t :which-key "Rust")
;;   "rr" 'cargo-process-run)

;; (setup (:pkg geiser-guile))

;; (setup (:pkg geiser)
;;   (setq geiser-default-implementation 'guile)
;;   (setq geiser-active-implementations '(guile))
;;   (setq geiser-repl-default-port 44555) ; For Gambit Scheme
;;   (setq geiser-implementations-alist '(((regexp "\\.scm$") guile))))

;; (rune/leader-keys
;;   "s"  '(:ignore t :which-key "Scheme")
;;   "sr" '(run-guile :which-key "Start a REPL"))

;; (setup (:pkg web-mode
;;   :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
;;   :config
;;   (setq-default web-mode-code-indent-offset 2)
;;   (setq-default web-mode-markup-indent-offset 2)
;;   (setq-default web-mode-attribute-indent-offset 2))

;; ;; 1. Start the server with `httpd-start'
;; ;; 2. Use `impatient-mode' on any buffer
;; (setup (:pkg impatient-mode
;;   :defer 5)

;; (setup (:pkg skewer-mode
;;   :defer 5)

;; ;; Run the webserver with command:
;; ;; M-x httpd-serve-directory 

;; (setup (:pkg simple-httpd
;;   :defer 5)

;; (setup (:pkg yaml-mode
;;   :mode "\\.ya?ml\\'")

(setup (:pkg smartparens)
  (:hook-into org-mode org-agenda-mode))

(setup (:pkg company))

;; (setup (:pkg company-box))

;; (use-package company
;;   :after lsp-mode
;;   :hook (lsp-mode . company-mode)
;;   :bind (:map company-active-map
;; 	      ("<tab>" . company-complete-selection))
;;   (:map lsp-mode-map
;; 	("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; (setup (:pkg flycheck :ensure)

;; (setup (:pkg neotree)
;; (setq neo-smart-open t
;;       neo-window-fixed-size nil)
;; (setq doom-neotree-enable-variable-pitch t)
;; (rune/leader-keys
;;   "n"  '(:ignore t :which-key "Neotree")
;;   "nt" '(neotree-toggle :which-key "Toggle neotree in file viewer")
;;   "nd" '(neotree-dir :which-key "Open a directory in Neotree"))

;; (setup (:pkg org-sidebar)

;; (rune/leader-keys
;;   "no" '(org-sidebar-tree :which-key "Tree Org"))

;; (setup (:pkg term)
;;   (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
;;   ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

;;   ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
;;   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(setup (:pkg vterm)
  (setq vterm-max-scrollback 10000)
  (advice-add 'evil-collection-vterm-insert :before #'vterm-reset-cursor-point))

(global-set-key (kbd "s-v") 'vterm)

;; (setup (:pkg vterm-toggle)

(rune/leader-keys
  "e"  '(:ignore t :which-key "Eshell")
  "es" '(eshell :which-key "Launch Eshell")
  "eh" '(counsel-esh-history :which-key "Eshell History"))

(setup (:pkg password-store))

(setq epa-pinentry-mode 'loopback)

;; Used to access passwords through emacs using Emacs's server-mode
(defun efs/lookup-password (&rest keys)
  (interactive)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(server-start)

(load-file "~/.emacs.d/desktop.el")

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
