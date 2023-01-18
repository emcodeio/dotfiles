(setq user-full-name "Evan Erickson"
      user-mail-address "evan@emcode.io")

(setq doom-theme 'doom-xcode)
(custom-theme-set-faces! 'doom-xcode
  ;;'(default :background "#1C1C1C")
  '(default :background "#151515")
  ;;'(default :background "#000000")
  )

(custom-theme-set-faces! 'doom-monokai-classic
  ;;'(default :background "#1C1C1C")
  '(default :background "#151515")
  ;;'(default :background "#000000")
  )

(custom-theme-set-faces! 'doom-dracula
  ;;'(default :background "#1C1C1C")
  '(default :background "#151515")
  ;;'(default :background "#000000")
  )

(custom-theme-set-faces! 'doom-palenight
  ;;'(default :background "#1C1C1C")
  '(default :background "#151515")
  ;;'(default :background "#000000")
  )

(setq doom-font (font-spec :family "Dank Mono" :size 13)
      doom-big-font (font-spec :family "Dank Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :weight 'light :size 13)
      doom-serif-font (font-spec :family "Iosevka Etoile" :weight 'light :size 13))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq display-line-numbers-type t)
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Comment or uncomment lines" "/" #'comment-line
       :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines" "t" #'toggle-truncate-lines))

(setq initial-frame-alist
      '(
        ;;(top . 1)
        ;;(left . 1)
        (width . 143)
        (height . 55)))

;; (use-package dashboard
;;   :init      ;; tweak dashboard config before loading it
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-banner-logo-title "\nKEYBINDINGS:\
;; \nFind file               (SPC .)     \
;; Open buffer list    (SPC b i)\
;; \nFind recent files       (SPC f r)   \
;; Open the eshell     (SPC e s)\
;; \nOpen dired file manager (SPC d d)   \
;; List of keybindings (SPC h b b)")
;;   ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
;;   (setq dashboard-startup-banner "~/.config/doom/doom-emacs-dash.png")  ;; use custom image as banner
;;   (setq dashboard-center-content nil) ;; set to 't' for centered content
;;   (setq dashboard-items '((recents . 5)
;;                           (agenda . 5 )
;;                           (bookmarks . 5)
;;                           (projects . 5)
;;                           (registers . 5)))
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (dashboard-modify-heading-icons '((recents . "file-text")
;;                                     (bookmarks . "book"))))

(set-fringe-style (quote (12 . 8)))

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Kill buffers matching" "o" #'kill-matching-buffers))

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Switch workspace buffer" "B" #'+vertico/switch-workspace-buffer))

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Switch buffer" "b" #'switch-to-buffer))

(map! :leader
      (:desc "Open Vterm" "v" #'vterm))

(map! :leader
      (:desc "Org Capture" "SPC" #'org-capture))

(map! :leader
      (:desc "Find file in project" "x" #'projectile-find-file))

(map! :leader
      (:desc "Find file in project" "X" #'doom/open-scratch-buffer))

(map! :leader
      (:desc "Switch workspace buffer" "<" #'+vertico/switch-workspace-buffer))

(map! :leader
      (:desc "Switch buffer" "," #'switch-to-buffer))

(map! :leader
      (:desc "Kill buffer" "k" #'kill-buffer))

(map! :leader
      (:desc "Quick Calculator" "C" #'quick-calc))

(map! :leader
      (:prefix ("s" . "search")
       :desc "Search Kill Ring" "k" #'consult-yank-pop))

(map! :leader
      (:prefix ("o" . "open")
       :desc "View Calendar" "g" #'calendar))

(global-set-key (kbd "C-s-'") 'evil-window-decrease-height)
(global-set-key (kbd "C-s-;") 'evil-window-increase-height)
(global-set-key (kbd "C-s-/") 'evil-window-decrease-width)
(global-set-key (kbd "C-s-.") 'evil-window-increase-width)

;; (map! :leader
;;       (:desc "Open in Finder" "z" #'reveal-in-osx-finder))

(after! evil
  ;; (define-key evil-insert-state-map
  ;;   (kbd "C-g")
  ;;   'evil-normal-state)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(require 'ace-window)
(setq aw-keys
      '(97 115 100 102 103 104 106 107 108))

(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define-global "vv" 'ace-window)

(global-auto-revert-mode 1)

(use-package! dired
  :commands (dired dired-jump))

(setq global-auto-revert-non-file-buffers t)
(setq delete-by-moving-to-trash t)
(setq large-file-warning-threshold nil)

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      ;; (:after dired
      ;;  (:map dired-mode-map
      ;;   :desc "Peep-dired image previews" "d p" #'peep-dired
      ;;   :desc "Dired view file" "d v" #'dired-view-file))
      )

;; (after! dired
;;   (evil-collection-define-key 'normal 'dired-mode-map
;;     "h" 'dired-up-directory
;;     "l" 'dired-find-file
;;     "o" 'xah-dired-sort))

(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file
  (kbd "o") 'xah-dired-sort)

(after! dired
  (setq dired-listing-switches "-agho --si --time-style long-iso --group-directories-first"))

(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
   Prompt for a choice.
   URL `http://ergoemacs.org/emacs/dired_sort.html'
   Version 2015-07-30"
  (interactive)
  (let (-sort-by -arg)
    (setq -sort-by (ido-completing-read "Sort by:" '( "time" "size" "name" "dir")))
    (cond
     ((equal -sort-by "name") (setq -arg "-agho --si --time-style long-iso "))
     ((equal -sort-by "time") (setq -arg "-agho --si --time-style long-iso -t"))
     ((equal -sort-by "size") (setq -arg "-agho --si --time-style long-iso -S"))
     ((equal -sort-by "dir") (setq -arg "-agho --si --time-style long-iso --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other -arg )))

(defun dired-get-size ()
 (interactive)
 (let ((files (dired-get-marked-files)))
   (with-temp-buffer
     (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
     (message "Size of all marked files: %s"
              (progn
                (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(require 'mm-util)
(require 'openwith)
(add-hook! 'after-init-hook #'openwith-mode)
(add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)

(setq openwith-associations
      (list
       (list
        (openwith-make-extension-regexp
         '("pdf" "heic" "png" "jpg" "flac" "jpeg" "gif"))
        "open"
        '(file))
       (list
        (openwith-make-extension-regexp
         '("mpg" "mpeg" "mp3" "mp4" "avi" "wmv" "wav" "mov" "flv" "ogm" "ogg" "mkv" "flac"))
        "open"
        '(file))))

(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)

(add-hook! 'dired-mode-hook #'dired-hide-dotfiles-mode)

(after! dired-hide-dotfiles
  (evil-collection-define-key 'normal 'dired-mode-map
    "f" 'dired-hide-dotfiles-mode))

;; (map! :leader
;;       :prefix "o"
;;       :desc "Dired at current location" "c" #'dired-jump
;;       ;; :desc "Open $HOME in dired" "h" (λ! (dired-switch-to-dir "~/"))
;;       ;; :desc "Open root in dired"  "r" (λ! (dired-switch-to-dir "/"))
;;       )

(after! org
  (setq org-directory "~/Documents/org/"
        org-agenda-files
        (list
         "inbox.org"
         "events.org"
         "projects.org"
         "db.org")
        org-ellipsis " ▽ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-item-bullet-alist '((?+ . ?‣) (?- . ?∙)) ; changes +/- symbols in item lists
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "NEXT(n)"           ; A task that is ready to be tackled
           "PROJ(p)"           ; A project that contains other tasks
           "WAIT(w)"           ; Something is holding up this task
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"           ; Task has been completed
           "CANCELLED(c)" ))) ; Task has been cancelled

  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-kill :after 'org-save-all-org-buffers)

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))))

(defun org-mode-<>-syntax-fix (start end)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "<\\|>" end t)
        (when (get-text-property (point) 'src-block)
          ;; This is a < or > in an org-src block
          (put-text-property (point) (1- (point))
                             'syntax-table (string-to-syntax "_")))))))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(add-hook! 'org-mode-hook #'efs/org-mode-visual-fill)

(defvar my-org-capture-filename nil
  "File name for org capture template.")

(defun my-org-capture ()
  "Read file name to capture to."
  (interactive)
  (setq my-org-capture-filename
        (read-file-name "Capture to: " "~/Documents/org"
                        nil t "inbox.org"))
  (call-interactively #'org-capture))

(after! org-capture
  (setq org-capture-templates
        `(("i" "Inbox" entry (file "inbox.org")
           "* TODO %?")
          ("e" "Event" entry (file+datetree "events.org")
           ,(concat "* %?\n"
             "<%<%Y-%m-%d %a %^{Time}>>")
           :time-prompt t)
          ("j" "Journal" checkitem (file+olp "projects.org" "SINGLES" "Journaling Ideas")
           "+ [ ] %?")
          ("m" "Inbox [mu4e]" entry (file "inbox.org")
           "* TODO Email: \"%a\"\n%i%?"
           :immediate-finish t)
           ;; ("n" "Note" entry (file "inbox.org")
           ;;  ,(concat "* Note (%a)\n"
           ;;           "%U\n" "%?"))
          ("n" "Next" entry (file "inbox.org")
           "* NEXT %?")
          ("p" "Project")
          ("pp" "Personal Project" entry (file+olp "projects.org" "PERSONAL")
           ;; replace %? with %^{Project title} to be prompted
           ,(concat "* PROJ %? [\/]\n"
                    ":PROPERTIES:\n"
                    ":CATEGORY:\n"
                    ":COOKIE_DATA: recursive todo\n"
                    ":END:\n"
                    "** Why?\n"
                          ":PROPERTIES:\n"
                          ":VISIBILITY: hide\n"
                          ":END:\n"
                    "** Notes\n"
                          ":PROPERTIES:\n"
                          ":VISIBILITY: hide\n"
                          ":END:\n"
                    "** Tasks\n"
                          ":PROPERTIES:\n"
                          ":VISIBILITY: content\n"
                          ":END:\n"))
          ("pw" "Personal Project" entry (file+olp "projects.org" "WORK")
           ,(concat "* PROJ %? [\/]\n"
                    ":PROPERTIES:\n"
                    ":CATEGORY:\n"
                    ":COOKIE_DATA: recursive todo\n"
                    ":END:\n"
                    "** Why?\n"
                          ":PROPERTIES:\n"
                          ":VISIBILITY: hide\n"
                          ":END:\n"
                    "** Notes\n"
                          ":PROPERTIES:\n"
                          ":VISIBILITY: hide\n"
                          ":END:\n"
                    "** Tasks\n"
                          ":PROPERTIES:\n"
                          ":VISIBILITY: content\n"
                          ":END:\n"))))
  (regexp-opt '("Tasks" "Notes"))
  (setq org-refile-targets
        '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
         ;;  ("inbox.org" :maxlevel . 3)
          ("projects.org" :regexp . "Single Personal Tasks")
          ("projects.org" :regexp . "Single Work Tasks")
          ))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil))

(defun eme/org-create-header-string (&optional org-element)
  "Returns a formated string. Abbreviated if it is over a certain length. Original otherwise."
  (let ((str (car (cdr (org-get-outline-path org-element)))))
    (if (< (length str) 20)
        (concat "[Project: " str "]")
      (concat "[Project: " (substring str 0 19) "...]"))))

(after! org-agenda
  (setq org-agenda-custom-commands
        '(("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))
          ("g" . "Get Things Done (GTD)")
          ("gw" "Work"
           ((agenda ""
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)))
            ;; (agenda nil
            ;;         ((org-agenda-entry-types '(:deadline))
            ;;          (org-agenda-format-date "")
            ;;          (org-deadline-warning-days 7)
            ;;          (org-agenda-skip-function
            ;;           '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
            ;;          (org-agenda-overriding-header "\nDeadlines")))
            (
             ;; todo "NEXT"
             tags-todo "+work/!NEXT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-prefix-format
               "        %-41(eme/org-create-header-string)")
              ;; (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-overriding-header "\nNext Tasks\n")))
            (tags-todo "+inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            ;; (tags "CLOSED>=\"<today>\""
            ;;       ((org-agenda-overriding-header "\nCompleted today\n")))
            (
             ;; todo "NEXT"
             tags-todo "+work/!TODO"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-prefix-format
               "        %-41(eme/org-create-header-string)")
              ;; (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-overriding-header "\nTodos\n")))

            (tags-todo "+work/!WAIT"
                       ((org-agenda-prefix-format
                         "        %-41(eme/org-create-header-string)")
                        ;; (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nWaiting\n")))
            ))
          ("gp" "Personal"
           ((agenda ""
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)))
            ;; (agenda nil
            ;;         ((org-agenda-entry-types '(:deadline))
            ;;          (org-agenda-format-date "")
            ;;          (org-deadline-warning-days 7)
            ;;          (org-agenda-skip-function
            ;;           '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
            ;;          (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "+personal/!NEXT"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'deadline))
                        (org-agenda-prefix-format
                         "        %-41(eme/org-create-header-string)")
                        ;; (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nNext Tasks\n")))
            (tags-todo "+inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            ;; (tags "CLOSED>=\"<today>\""
            ;;       ((org-agenda-overriding-header "\nCompleted today\n")))
            (
             ;; todo "NEXT"
             tags-todo "+personal/!TODO"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-prefix-format
               "        %-41(eme/org-create-header-string)")
              ;; (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-overriding-header "\nTodos\n")))
            (tags-todo "+personal/!WAIT"
                       ((org-agenda-prefix-format
                         "        %-41(eme/org-create-header-string)")
                        ;; (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nWaiting\n")))
            )))))

(defun org-archive-done (&optional arg)
  (org-todo 'done))

(advice-add 'org-archive-subtree :before 'org-archive-done)

(map! :leader
      (:prefix ("n" . "notes")
       :desc "View GTD Work" "w" #'(lambda ()
                                      (interactive)
                                      (org-agenda nil "gw"))
       :desc "View GTD Personal" "p" #'(lambda ()
                                      (interactive)
                                      (org-agenda nil "gp"))))

(defun eme/renew-org-buffer ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-maybe-redo)))))

(run-with-timer 3 (* 3 60) #'eme/renew-org-buffer)

(add-hook! 'org-mode-hook #'org-auto-tangle-mode)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(after! org-mode
  (setq org-latex-caption-above nil)
        org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(require 'org-present)

(defun my/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun my/org-present-hook ()
  (setq-local face-remapping-alist '((header-line (:height 4.0) variable-pitch)))
  (setq org-hide-emphasis-markers t)
  (setq header-line-format " ")
  (display-line-numbers-mode -1)
  (mixed-pitch-mode 1)
  (doom-big-font-mode 1)
  (org-display-inline-images)
  (my/org-present-prepare-slide))

(defun my/org-present-quit-hook ()
  (setq org-hide-emphasis-markers nil)
  (setq header-line-format nil)
  (display-line-numbers-mode 1)
  (mixed-pitch-mode -1)
  (doom-big-font-mode -1)
  ;; (org-present-small)
  (org-remove-inline-images))

(defun my/org-present-prev ()
  (interactive)
  (org-present-prev)
  (my/org-present-prepare-slide))

(defun my/org-present-next ()
  (interactive)
  (org-present-next)
  (my/org-present-prepare-slide))

(add-hook 'org-present-mode-hook #'my/org-present-hook)
(add-hook 'org-present-mode-quit-hook #'my/org-present-quit-hook)
(bind-keys :package org-present :map org-present-mode-keymap
           ("C-j" . org-present-next)
           ("C-k" . org-present-prev))

(map! :map org-mode-map
      :localleader
      :desc "start org present" "v" #'org-present)

(defun eme/capture-mail-headers (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "m")
  (mu4e-headers-mark-for-refile))

(defun eme/capture-mail-view (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "m")
  (mu4e-view-mark-for-refile))

(defun eme/store-link-to-mu4e-query ()
  (interactive)
  (let ((mu4e-org-link-query-in-headers-mode t))
    (call-interactively 'org-store-link)))

(eval-and-compile
  (add-to-list 'load-path "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e"))
(require 'mu4e)
(require 'smtpmail)
      ;; installed this with homebrew
(setq mu4e-mu-binary (executable-find "mu")
      ;; mu4e mail directory:
      mu4e-maildir "~/.maildir"
      ;; this command is called to sync imap servers:
      mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
      ;; how often to call it in seconds:
      mu4e-update-interval (* 5 60)
      ;; run in background
      mu4e-index-update-in-background t
      ;; save attachment to ~/inbox by default
      mu4e-attachment-dir "~/inbox"
      ;; rename files when moving - needed for mbsync:
      mu4e-change-filenames-when-moving t
      ;; Make sure plain text mails flow correctly for recipients
      mu4e-compose-format-flowed t
      ;; list of email adresses:
      mu4e-user-mail-address-list '("evan_e@icloud.com"
                                    "evan@emcode.io"
                                    "evan.erksn@gmail.com"
                                    "ericenna@gmail.com"
                                    "eerickson@phasechange.ai"))

(add-to-list 'mu4e-headers-actions
             '("org capture" . eme/capture-mail-headers) t)
(add-to-list 'mu4e-view-actions
             '("org capture" . eme/capture-mail-view) t)

;; (add-to-list 'mu4e-bookmarks
;;              (make-mu4e-bookmark
;;               :name "Inbox - Work"
;;               :query "maildir:/work/INBOX"
;;               :key ?w))
;; (add-to-list 'mu4e-bookmarks
;;              (make-mu4e-bookmark
;;               :name "Inbox - Spam"
;;               :query "maildir:/ericenna-gmail/INBOX"
;;               :key ?s))
;; (add-to-list 'mu4e-bookmarks
;;              (make-mu4e-bookmark
;;               :name "Inbox - Gamil"
;;               :query "maildir:/evan.erksn-gmail/INBOX"
;;               :key ?g))
;; (add-to-list 'mu4e-bookmarks
;;              (make-mu4e-bookmark
;;               :name "Inbox - iCloud"
;;               :query "maildir:/icloud/INBOX"
;;               :key ?a))
;; (add-to-list 'mu4e-bookmarks
;;              (make-mu4e-bookmark
;;               :name "All Inboxes"
;;               :query (concat
;;                       "m:/icloud/INBOX "
;;                       "or m:/evan.erksn-gmail/INBOX "
;;                       "or m:/ericenna-gmail/INBOX "
;;                       "or m:/work/INBOX")
;;               :key ?i))

(add-to-list 'mu4e-bookmarks
             '(:name "Inbox - Work"
               :query "maildir:/work/INBOX"
               :key ?w))
(add-to-list 'mu4e-bookmarks
             '(:name "Inbox - Spam"
               :query "maildir:/ericenna-gmail/INBOX"
               :key ?s))
(add-to-list 'mu4e-bookmarks
             '(:name "Inbox - Gamil"
               :query "maildir:/evan.erksn-gmail/INBOX"
               :key ?g))
(add-to-list 'mu4e-bookmarks
             '(:name "Inbox - iCloud"
               :query "maildir:/icloud/INBOX"
               :key ?a))
(add-to-list 'mu4e-bookmarks
             '(:name "All Inboxes"
               :query "m:/icloud/INBOX or m:/evan.erksn-gmail/INBOX or m:/ericenna-gmail/INBOX or m:/work/INBOX"
               :key ?i))

(setq mu4e-contexts
      (list
       ;; Emcode account
       (make-mu4e-context
        :name "Emcode"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/icloud" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "evan@emcode.io" )
                (user-full-name . "Evan Erickson")
                (mu4e-drafts-folder . "/icloud/Drafts")
                (mu4e-refile-folder . "/icloud/Archive")
                (mu4e-sent-folder . "/icloud/Sent Messages")
                (mu4e-trash-folder . "/icloud/Deleted Messages")))
       ;; iCloud acount
       (make-mu4e-context
        :name "iCloud"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/icloud" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address  . "evan_e@icloud.com" )
                (user-full-name     . "Evan Erickson")
                (mu4e-drafts-folder . "/icloud/Drafts")
                (mu4e-refile-folder . "/icloud/Archive")
                (mu4e-sent-folder   . "/icloud/Sent Messages")
                (mu4e-trash-folder  . "/icloud/Deleted Messages")))
       ;; Gmail account
       (make-mu4e-context
        :name "Gmail"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/evan.erksn-gmail" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address  . "evan.erksn@gmail.com")
                (user-full-name     . "Evan Erickson")
                (mu4e-drafts-folder . "/evan.erksn-gmail/[Gmail]/Drafts")
                (mu4e-sent-folder   . "/evan.erksn-gmail/[Gmail]/Sent Mail")
                (mu4e-refile-folder . "/evan.erksn-gmail/[Gmail]/All Mail")
                (mu4e-trash-folder  . "/evan.erksn-gmail/[Gmail]/Trash")))
       ;; Spam gmail account
       (make-mu4e-context
        :name "Spam"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/ericenna-gmail" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address  . "ericenna@gmail.com")
                (user-full-name     . "Evan Erickson")
                (mu4e-drafts-folder . "/ericenna-gmail/[Gmail]/Drafts")
                (mu4e-sent-folder   . "/ericenna-gmail/[Gmail]/Sent Mail")
                (mu4e-refile-folder . "/ericenna-gmail/[Gmail]/All Mail")
                (mu4e-trash-folder  . "/ericenna-gmail/[Gmail]/Trash")))
       ;; Work account
       (make-mu4e-context
        :name "Work"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
        ;; :name "Work"
        ;; :enter-func
        ;; (lambda () (mu4e-message "Enter eerickson@phasechange.ai context"))
        ;; :leave-func
        ;; (lambda () (mu4e-message "Leave eerickson@phasechange.ai context"))
        ;; :match-func
        ;; (lambda (msg)
        ;;   (when msg
        ;;     (or (mu4e-message-contact-field-matches msg
        ;;                                             :to "eerickson@phasechange.ai")
        ;;         (mu4e-message-contact-field-matches msg
        ;;                                             :to "company@phasechange.ai"))))
        :vars '((user-mail-address  . "eerickson@phasechange.ai")
                (user-full-name     . "Evan Erickson")
                (mu4e-drafts-folder . "/work/Drafts")
                (mu4e-sent-folder   . "/work/Sent Items")
                (mu4e-refile-folder . "/work/Archive")
                (mu4e-trash-folder  . "/work/Trash"))))
      mu4e-context-policy 'pick-first  ;; start with the first (default) context;
      mu4e-compose-context-policy 'ask ;; ask for context if no context matches;
      )

;; SENDING SETTINGS
;; gpg encryptiom & decryption:
;; this can be left alone
(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)
(auth-source-forget-all-cached)

;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t
      send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it
      ;; send program.
      sendmail-program (executable-find "msmtp")
      ;; select the right sender email from the context.
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header
      ;; turn off Org-msg-mode by default
      mu4e-compose--org-msg-toggle-next nil
      mu4e-compose-format-flowed t)

;; mu4e cc & bcc
(add-hook! 'mu4e-compose-mode-hook
  (defun timu/add-cc-and-bcc ()
    "My Function to automatically add Cc & Bcc: headers.
             This is in the mu4e compose mode."
    (save-excursion (message-add-header "Cc:\n"))
    (save-excursion (message-add-header "Bcc:\n"))))
;; mu4e address completion
(add-hook! 'mu4e-compose-mode-hook 'company-mode)
(add-hook! 'mu4e-compose-mode-hook (lambda () (use-hard-newlines -1)))

(setq mu4e-alert-interesting-mail-query
      (concat
       "flag:unread"
       " and m:/icloud/INBOX"
       " or m:/evan.erksn-gmail/INBOX"
       " or m:/ericenna-gmail/INBOX"
       " or m:/work/INBOX"))
(add-hook! 'after-init-hook #'mu4e-alert-enable-mode-line-display)

(require 'org-mime)
(setq org-mime-export-options
      '(:section-numbers nil
        :with-author nil
        :with-toc nil))
(add-hook! 'message-send-hook 'org-mime-confirm-when-no-multipart)

(setq +mu4e-main-bullet "‣")
(setq mu4e-headers-thread-child-prefix '("├>" . "├─➤ ")
      mu4e-headers-thread-last-child-prefix '("└>" . "└─➤ ")
      mu4e-headers-thread-orphan-prefix '("┬>" . "┬─➤ ")
      mu4e-headers-thread-single-orphan-prefix '("─>" . "──➤ ")
      ;; The following two should have the same width.
      mu4e-headers-thread-connection-prefix '("│" . "│ ")
      mu4e-headers-thread-blank-prefix '(" " . " "))

(map! :leader
      (:desc "Compose email" "e" #'+mu4e/compose))

(map! :map mu4e-compose-mode-map
      :localleader
      :desc "convert to html email" "h" #'org-mime-htmlize
      :desc "edit email in org buffer" "o" #'org-mime-edit-mail-in-org-mode)

(setq projectile-project-search-path '("~/dev/"))

(setq display-line-numbers-type t)
(map! :leader
      (:prefix ("l" . "lsp")
       :desc "Jump to method definition" "d" #'lsp-find-definition
       :desc "Show method references" "r" #'lsp-find-references))

(map! :map dap-mode-map
      :leader
      :prefix ("D" . "dap")
      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap disconnect"    "x" #'dap-disconnect
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug"         "s" #'dap-debug

      ;; debug
      :prefix ("Dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("De" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("Db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint delete all"  "d" #'dap-breakpoint-delete-all

      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)

;; (after! dap-mode
;;   (setq dap-python-debugger 'debugpy))

(map! :map clojure-mode-map
      :localleader
      :desc "Slurp foward" "s" #'paredit-forward-slurp-sexp
      :desc "Slurp backward" "S" #'paredit-backward-slurp-sexp
      :desc "Barf backward" "b" #'paredit-forward-barf-sexp
      :desc "Barf backward" "B" #'paredit-backward-barf-sexp
      :desc "Kill parens" "k" #'paredit-kill)

(map! :map cider-repl-mode-map
      :localleader
      :desc "cider-repl-history" "h" #'cider-repl-history
      :desc "Slurp foward" "s" #'paredit-forward-slurp-sexp
      :desc "Slurp backward" "S" #'paredit-backward-slurp-sexp
      :desc "Barf backward" "b" #'paredit-forward-barf-sexp
      :desc "Barf backward" "B" #'paredit-backward-barf-sexp
      :desc "Kill parens" "k" #'paredit-kill)

(add-to-list 'auto-mode-alist
             '("\\.cob\\'" . (lambda ()
                               ;; add major mode setting here, if needed, for example:
                               ;; (text-mode)
                               (cobol-mode)
                               (column-enforce-mode))))

(use-package! python-mode
  :hook (python-mode . run-python)
  :hook (python-mode . lsp-deferred)
  :custom
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy))

(add-hook! 'java-mode-hook #'(lambda ()
                               (gradle-mode 1)))

(defun build-and-run ()
  (interactive)
  (gradle-run "build run"))

(map! :after java
      :map gradle-mode-map
      :leader
      :prefix ("j" . "java")
      ;; basics
      :desc "Gradel Build Run" "r" #'build-and-run)

(add-to-list 'auto-mode-alist
             '("\\.fom\\'" . (lambda ()
                               ;; add major mode setting here, if needed, for example:
                               ;; (text-mode)
                               (json-mode))))
