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

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setup (:pkg undo-tree)
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

(setup (:pkg evil)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  (evil-mode))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setup (:pkg evil-collection)
  (evil-collection-init))

(setup (:pkg general)
  (general-create-definer space-keys 
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")

  (space-keys
   "SPC" '(find-file :which-key "find file")))

(setq inhibit-startup-message t)
(setq visible-bell nil)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(set-face-attribute 'default nil
                    :family "Jetbrains Mono"
                    :height 80
                    :weight 'normal
                    :width  'normal)

(setup (:pkg doom-themes))
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-molokai t)

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; (setup (:pkg all-the-icons))

;; (setup (:pkg all-the-icons-completion)
;;   (all-the-icons-completion-mode))

(setup (:pkg doom-modeline)
  (:hook-into after-init-hook)
  (:option doom-modeline-height 10
	   doom-modeline-irc t)
  (setq display-time-day-and-date t)
  (setq display-time-string-forms '((format-time-string "%H:%M:%S" now)))
  (setq display-time-interval 1)
  (display-time-mode 1))

(start-process-shell-command "scroll speed" nil "xset r rate 200 50")

(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
		dashboard-mode-hook
		vterm-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setup (:pkg rainbow-delimiters)
  (:hook-into prog-mode))

(space-keys
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

(add-hook 'org-mode-hook (lambda ()(org-toggle-pretty-entities)(flyspell-mode)))

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

(setup (:pkg org-appear)
  (:hook-into org-mode)
  (setq org-hide-emphasis-markers t) ;; A default setting that needs to be t for org-appear
  (setq org-appear-autoemphasis t)  ;; Enable org-appear on emphasis (bold, italics, etc)
  (setq org-appear-autolinks t) ;; Enable on links
  (setq org-appear-autosubmarkers t)) ;; Enable on subscript and superscript

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

(space-keys
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
  (require 'org-auto-tangle)
  (add-hook 'org-mode-hook 'org-auto-tangle-mode))

(defun my-save-word ()
  "Save a word to a dictionary that is stored in ~/.aspell.en.pws"
  (interactive)
  (let ((current-location (point))
	(word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(space-keys
  "f"  '(:ignore t :which-key "Flyspell")
  "fm" '(flyspell-mode :which-key "Start Flyspell-Mode")
  "fa" '(my-save-word :which-key "Add word to Flyspell"))

(setq-default abbrev-mode t) ;; Enable abbrev-mode

(read-abbrev-file "~/.emacs.d/abbrev_defs")

(setq save-abbrevs 'silent)

(setq abbrev-file-name
      "~/.emacs.d/abbrev_defs")

(space-keys
 "s"  '(:ignore t :which-key "Abbrev")
 "sa" '(add-global-abbrev :which-key "Add word to abbrev globally"))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(run-at-time nil (* 5 60) 'recentf-save-list)

(space-keys
  "t" '(counsel-recentf :which-key "Recent files"))

(setup (:pkg password-store))

(setq epa-pinentry-mode 'loopback)

;; Used to access passwords through emacs using Emacs's server-mode
(defun efs/lookup-password (&rest keys)
  (interactive)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(space-keys
  "TAB" '(comment-dwim :which-key "comment lines"))

(space-keys
  "d"  '(:ignore t :which-key "Files")
  "dt" '((lambda() (interactive) (find-file "~/documents/Home/TODO.org")) :which-key "TODO")
  "ds" '((lambda() (interactive) (find-file "~/documents/Home/Reminders.org")) :which-key "Schedule")
  "dh" '((lambda() (interactive) (find-file "~/documents/School/Homework.org")) :which-key "Homework")
  "dr" '((lambda() (interactive) (find-file "~/documents/School/School-Reminders.org")) :which-key "Reminders"))

(space-keys
  "c"  '(:ignore t :which-key "Files")
  "ce" '((lambda() (interactive) (find-file "~/dotfiles/org/emacs.org")) :which-key "Emacs config")
  "cd" '((lambda() (interactive) (find-file "~/dotfiles/org/desktop.org")) :which-key "Desktop config")
  "cs" '((lambda() (interactive) (find-file "~/dotfiles/org/system.org")) :which-key "System config")
  "cp" '((lambda() (interactive) (find-file "~/dotfiles/org/programs.org")) :which-key "Programs config"))

(require 'erc) ;; Notifications require this to be required
(setq erc-server "irc.libera.chat"
      erc-nick "Haider"
      erc-user-full-name "Haider Mirza"
      erc-rename-buffers t
      erc-kill-buffer-on-part t
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20
      erc-auto-query 'bury
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

(space-keys
  "i"  '(:ignore t :which-key "IRC")
  "ii" '(chat/connect-irc :which-key "launch IRC")
  "ib" '(erc-switch-to-buffer :which-key "Switch Buffer"))

(setup (:pkg emojify)
  (add-hook 'erc-mode-hook #'global-emojify-mode))

(space-keys
  "a"  '(:ignore t :which-key "Emojify") ;; I know a has no correlation but Im running out of space ok.
  "ai" '(emojify-insert-emoji :which-key "Insert Emoji"))

(setup (:pkg unicode-fonts))

(setup (:pkg magit))

(space-keys
  "m"  '(:ignore t :which-key "Magit")
  "ms" '(magit-status :which-key "Magit Status"))

(setup (:pkg yasnippet)
  (setq yas-snippet-dirs '("~/.emacs.d/etc/yasnippet/snippets"
                           "~/guix/etc/snippets"))
  (yas-global-mode))

(setup (:pkg company))

(space-keys
  "e"  '(:ignore t :which-key "E-Lisp")
  "el" '(eval-last-sexp :which-key "Evaluate last sexpression")
  "er" '(eval-region :which-key "Evaluate elisp in region"))

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



(setup (:pkg vterm)
  (setq vterm-max-scrollback 10000)
  (advice-add 'evil-collection-vterm-insert :before #'vterm-reset-cursor-point))

(global-set-key (kbd "s-v") 'vterm)

(setup (:pkg elfeed-goodies :straight t)
  (setq elfeed-goodies/entry-pane-size 0.5)
  (elfeed-goodies/setup))

(setup (:pkg elfeed)
  (setq elfeed-feeds
        '(("https://www.archlinux.org/feeds/news/" linux)
          ("https://www.reddit.com/r/emacs.rss" reddit linux)
          ("https://www.reddit.com/r/linux.rss" reddit linux)
          ("https://sachachua.com/blog/feed/" linux emacs))))

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

(space-keys
  "n"  '(:ignore t :which-key "Notmuch")
  "nu" '(notmuch/update :which-key "Notmuch Update")
  "nh" '(notmuch-hello-update :which-key "Notmuch Update Hello buffer")
  "nt" '(notmuch/trash :which-key "Notmuch Trash")
  "na" '(notmuch/archive :which-key "Notmuch Archive")
  "nr" '(notmuch/read :which-key "Notmuch Read"))

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

(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

(setup (:pkg vertico)
  (vertico-mode))

(setup savehist
  (savehist-mode 1))

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)

(server-start)

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

(defun exwm/picom ()
  (interactive)
  (start-process-shell-command "picom" nil "picom"))

(defun exwm/run-xmodmap ()
  (interactive)
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/.xmodmap"))

(defun exwm/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil  "feh --bg-scale ~/Wallpapers/main.png"))

(defun exwm/unclutter ()
  (interactive)
  (start-process-shell-command "unclutter" nil "unclutter -idle 0.01 -root"))

(defun exwm/kill-unclutter ()
  (interactive)
  (start-process-shell-command "kill unclutter" nil "pkill unclutter"))

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

(require 'desktop-environment)
(desktop-environment-volume-set "50%")

(defun exwm/exwm-init-hook ()
  (exwm-workspace-switch-create 1)
  (exwm/unclutter)
  (exwm/set-wallpaper)
  (exwm/picom)
  (start-process-shell-command "scroll speed" nil "xset r rate 200 50")
  (exwm/run-in-background "dunst")

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
  (exwm-workspace-switch-create 6)
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
  (start-process-shell-command "dunst" nil "dunstify Progress: -h int:value:100")

  (message "Post Initialization script...")
  (sleep-for 7)
  (start-process-shell-command "sound-effect" nil "mpv --no-video /home/haider/do-not-delete/startup.mp3")
  (let ((password (read-passwd "ERC Password: ")))
    (exwm-workspace-switch-create 7)
    (chat/connect-irc password)))

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

(setup (:pkg exwm)
  (setq exwm-workspace-number 9)
  (add-hook 'exwm-update-class-hook #'exwm/exwm-update-class)
  (add-hook 'exwm-update-title-hook #'exwm/exwm-update-title)

  (setq exwm-input-global-keys
	`(([?\s-r] . exwm-reset)
	  ([?\s-f] . exwm-layout-toggle-fullscreen)
	  ([?\s-z] . exwm-layout-toggle-mode-line)
	  ([?\s-b] . consult-buffer)
	  ([\f6] . desktop-environment-toggle-mute)
	  ([\f7] . desktop-environment-volume-decrement)
	  ([\f8] . desktop-environment-volume-increment)
	  ([?\s-e] . (lambda () (interactive) (dired "~")))
	  ([?\s-q] . (lambda () (interactive) (kill-buffer)))

	  ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))

	  ([?\s-w] . exwm-workspace-switch)
	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9))))

  (add-hook 'exwm-init-hook #'exwm/exwm-init-hook)
  (exwm-enable))
