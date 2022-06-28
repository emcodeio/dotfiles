(map! :leader
      (:prefix ("f" . "file")
       :desc "Load org file" "b" #'org-babel-load-file))

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

(setq doom-font (font-spec :family "Dank Mono" :size 15)
      doom-big-font (font-spec :family "Dank Mono" :size 24))
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

(use-package! dired
    :commands (dired dired-jump)
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-up-directory
      "l" 'dired-find-file
      "o" 'xah-dired-sort))
(after! dired
  (setq dired-listing-switches "-agho --si --time-style long-iso --group-directories-first"))

(setq global-auto-revert-non-file-buffers t)
(setq delete-by-moving-to-trash nil)
(setq large-file-warning-threshold nil)

(use-package! openwith
  :init
  ;; Prevents openwith from messing up email attachments
  (require 'mm-util)
  :config
    (setq openwith-associations
      (list
       (list (openwith-make-extension-regexp
              '("pdf" "heic" "png" "jpg" "flac"
                "jpeg" "gif"))
             "open"
             '(file))
       (list (openwith-make-extension-regexp
              '("mpg" "mpeg" "mp3" "mp4"
                "avi" "wmv" "wav" "mov" "flv"
                "ogm" "ogg" "mkv" "flac"))
             "open"
             '(file))
       ;; '("\\.chm" "kchmviewer" (file))
       ))
    (add-hook! 'after-init-hook #'openwith-mode)
    ;; Prevents openwith from messing up email attachments
    (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler))

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

(use-package! dired+
  :config
  (diredp-toggle-find-file-reuse-dir 1)
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil))

(use-package! dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
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
         "projects.org")
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
           "CANCELLED(c)" )))) ; Task has been cancelled

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package! visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(after! org-capture
  (setq org-capture-templates
        `(("i" "Inbox" entry (file "inbox.org")
           "* TODO %?\n %U")
           ("d" "DONE" entry (file "projects.org")
            "* DONE %a" :empty-lines 1)
          ("d")
          ("e" "Inbox [mu4e]" entry (file "inbox.org")
           "* TODO Process \"%a\" %?\n %U")
          ;; ("n" "Note" entry (file "inbox.org")
          ;;  ,(concat "* Note (%a)\n"
          ;;           "%U\n" "%?"))
          ("p" "Project" entry (file "projects.org")
           ,(concat "* PROJ %^{Project title} [\/]\n"
                    ":PROPERTIES:\n"
                    ":CATEGORY:\n"
                    ":VISIBILITY: hide\n"
                    ":COOKIE_DATA: recursive todo\n"
                    ":END:\n"
                    "** Why?\n"
                          ":PROPERTIES:\n"
                          ":VISIBILITY: hide\n"
                          ":END:\n"
                    "** Information\n"
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
        '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil))

(after! org-agenda
  (setq org-agenda-custom-commands
        '(("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))
          ("g" "Get Things Done (GTD)"
           ((agenda ""
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)))
            (todo "NEXT"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks\n")))
            (agenda nil
                    ((org-agenda-entry-types '(:deadline))
                     (org-agenda-format-date "")
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                     (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n"))))))))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(use-package! mu4e
  :load-path "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e"
  :config
  (require 'smtpmail)
  ;; installed this with homebrew
  (setq mu4e-mu-binary (executable-find "mu"))
  ;; mu4e mail directory:
  (setq mu4e-maildir "~/.maildir")
  ;; this command is called to sync imap servers:
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  ;; how often to call it in seconds:
  (setq mu4e-update-interval (* 5 60))
  ;; save attachment to ~/inbox by default
  (setq mu4e-attachment-dir "~/inbox")
  ;; rename files when moving - needed for mbsync:
  (setq mu4e-change-filenames-when-moving t)
  ;; list of email adresses:
  (setq mu4e-user-mail-address-list '("evan_e@icloud.com"
                                      "evan@emcode.io"
                                      "evan.erksn@gmail.com"
                                      "ericenna@gmail.com"
                                      "eerickson@phasechange.ai"))

  ;; make bookmarks
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Inbox - Work"
                :query "maildir:/work/INBOX"
                :key ?w))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Inbox - Spam"
                :query "maildir:/ericenna-gmail/INBOX"
                :key ?s))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Inbox - Gamil"
                :query "maildir:/evan.erksn-gmail/INBOX"
                :key ?g))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Inbox - iCloud"
                :query "maildir:/icloud/INBOX"
                :key ?a))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "All Inboxes"
                :query "m:/icloud/INBOX or m:/evan.erksn-gmail/INBOX or m:/ericenna-gmail/INBOX or m:/work/INBOX"
                :key ?i))

  ;; creating contexts
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
                   (mu4e-trash-folder  . "/work/Trash")))))

  (setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
  (setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;

  ;; SENDING SETTINGS
  ;; gpg encryptiom & decryption:
  ;; this can be left alone
  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (auth-source-forget-all-cached)

  ;; don't keep message compose buffers around after sending:
  (setq message-kill-buffer-on-exit t)
  ;; send function:
  ;; (setq send-mail-function 'sendmail-send-it)
  (setq send-mail-function 'sendmail-send-it
        message-send-mail-function 'sendmail-send-it
        ;; send program:
        sendmail-program (executable-find "msmtp")
        ;; select the right sender email from the context.
        mail-specify-envelope-from t
        mail-envelope-from 'header
        message-sendmail-envelope-from 'header)

  ;; Turn off Org-msg-mode by default
  (setq mu4e-compose--org-msg-toggle-next nil)

  ;; mu4e cc & bcc
  (add-hook 'mu4e-compose-mode-hook
            (defun timu/add-cc-and-bcc ()
              "My Function to automatically add Cc & Bcc: headers.
               This is in the mu4e compose mode."
              (save-excursion (message-add-header "Cc:\n"))
              (save-excursion (message-add-header "Bcc:\n"))))
  ;; mu4e address completion
  (add-hook 'mu4e-compose-mode-hook 'company-mode)

  ;; COSMETICS
  (setq +mu4e-main-bullet "‣")
  (setq mu4e-headers-thread-child-prefix '("├>" . "├─➤ ")
        mu4e-headers-thread-last-child-prefix '("└>" . "└─➤ ")
        mu4e-headers-thread-orphan-prefix '("┬>" . "┬─➤ ")
        mu4e-headers-thread-single-orphan-prefix '("─>" . "──➤ ")
        ;; The following two should have the same width.
        mu4e-headers-thread-connection-prefix '("│" . "│ ")
        mu4e-headers-thread-blank-prefix '(" " . " ")))

;; (defun em/send ()
;;   "Prompt user for email type"
;;   (interactive)
;;   (let (n)
;;     (setq n ())))

(use-package! org-mime
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil))
  (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart)
  ;; (add-hook 'org-mime-html-hook
  ;;           (lambda ()
  ;;             (org-mime-change-element-style
  ;;              "pre" (format "background-color %s; padding: 0.5em;"
  ;;                            "#232323"))))
  )

(map! :leader
      (:desc "Compose email" "e" #'+mu4e/compose))

(map! :map mu4e-compose-mode-map
      :localleader
      :desc "convert to html email" "h" #'org-mime-htmlize
      :desc "edit email in org buffer" "o" #'org-mime-edit-mail-in-org-mode)

(setq display-line-numbers-type t)
(map! :leader
      (:prefix ("l" . "lsp")
       :desc "Jump to method definition" "d" #'lsp-find-definition
       :desc "Show method references" "r" #'lsp-find-references))

(setq mmm-global-mode 'maybe)

(defun my-mmm-markdown-auto-class (lang &optional submode)
  "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "^```" lang "[\n\r]+"))
        (back "^```"))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)))

(mapc 'my-mmm-markdown-auto-class
      '("awk" "bibtex" "c" "cpp" "css" "html" "latex" "lisp" "makefile"
        "markdown" "python" "r" "ruby" "sql" "stata" "xml" "cobol"))

(my-mmm-markdown-auto-class "fortran" 'f90-mode)
(my-mmm-markdown-auto-class "perl" 'cperl-mode)
(my-mmm-markdown-auto-class "shell" 'shell-script-mode)

(use-package! ace-window
  :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package! key-chord
  :config
    (key-chord-mode 1)
    (setq key-chord-two-keys-delay 0.05)
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (key-chord-define-global "vv" 'ace-window))

(use-package! evil
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq initial-frame-alist
      '(
        ;;(top . 1)
        ;;(left . 1)
        (width . 143)
        (height . 55)))

(defun org-mode-<>-syntax-fix (start end)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "<\\|>" end t)
    (when (get-text-property (point) 'src-block)
      ;; This is a < or > in an org-src block
      (put-text-property (point) (1- (point))
                 'syntax-table (string-to-syntax "_")))))))

;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "‣"))))))

;; (after! org
;;   (add-hook 'org-mode-hook (λ! (org-bullets-mode 1)))
;;   (add-hook 'org-mode-hook
;;       (λ!
;;         (setq syntax-propertize-function 'org-mode-<>-syntax-fix)
;;         (syntax-propertize (point-max))))
;;   (setq org-directory "~/Org/"
;;         org-agenda-files '("~/Org/agenda.org")
;;         org-default-notes-file (expand-file-name "notes.org" org-directory)
;;         org-ellipsis " ∇"
;;         org-log-done 'time
;;         org-journal-dir "~/Org/journal/"
;;         org-journal-date-format "%B %d, %Y (%A) "
;;         org-journal-file-format "%Y-%m-%d.org"
;;         ;; org-hide-emphasis-markers nil
;;         ;; ex. of org-link-abbrev-alist in action
;;         ;; [[arch-wiki:Name_of_Page][Description]]
;;         org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
;;           '(("google" . "http://www.google.com/search?q=")
;;             ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
;;             ("ddg" . "https://duckduckgo.com/?q=")
;;             ("wiki" . "https://en.wikipedia.org/wiki/"))
;;         org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
;;           '((sequence
;;              "TODO(t)"           ; A task that is ready to be tackled
;;              "BLOG(b)"           ; Blog writing assignments
;;              "GYM(g)"            ; Things to accomplish at the gym
;;              "PROJ(p)"           ; A project that contains other tasks
;;              "VIDEO(v)"          ; Video assignments
;;              "WAIT(w)"           ; Something is holding up this task
;;              "|"                 ; The pipe necessary to separate "active" states and "inactive" states
;;              "DONE(d)"           ; Task has been completed
;;              "CANCELLED(c)" ))
;;         org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))) ; Task has been cancelled

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(map! :leader
      (:desc "Open in Finder" "z" #'reveal-in-osx-finder))

(setq projectile-project-search-path '("~/dev/"))
(setq org-latex-caption-above nil)
;; (setq org-src-fontify-natively t)

(global-auto-revert-mode 1)

;; (when (executable-find "ipython")
;;  (setq python-shell-interpreter "ipython"))

(add-to-list 'auto-mode-alist
             '("\\.cob\\'" . (λ! ()
                               ;; add major mode setting here, if needed, for example:
                               ;; (text-mode)
                               (cobol-mode)
                               (column-enforce-mode))))

;; (dap-register-debug-template
;;   "Python :: Run (test)"
;;   (list :type "python"
;;         :cwd (file-name-directory buffer-file-name)
;;         :module "pytest"
;;         :request "launch"
;;         ;; :target-module (expand-file-name "~/src/myapp/.env/bin/myapp")
;;         :debugger 'debugpy
;;         :name "Python :: Run (test)"))

(use-package python-mode
  :hook (python-mode . run-python)
  :hook (python-mode . lsp-deferred)
  :custom
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy))

(add-hook 'java-mode-hook #'(lambda() (gradle-mode 1)))

(defun build-and-run ()
	(interactive)
	(gradle-run "build run"))

(map! :after java
      :map gradle-mode-map
      :leader
      :prefix ("j" . "java")
      ;; basics
      :desc "Gradel Build Run"          "r" #'build-and-run)

;; (define-key gradle-mode-map (kbd "C-c C-r") 'build-and-run)

(map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
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
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint delete all"  "d" #'dap-breakpoint-delete-all
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)

;; (after! dap-mode
;;   (setq dap-python-debugger 'debugpy))

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Kill buffers matching" "o" #'kill-matching-buffers))

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Switch workspace buffer" "B" #'+vertico/switch-workspace-buffer))

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Switch buffer" "b" #'switch-to-buffer))

(set-fringe-style (quote (12 . 8)))

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
      (:prefix ("s" . "search")
       :desc "Search Kill Ring" "k" #'consult-yank-pop))

;; (require 'eaf-demo)
;; (require 'eaf-file-sender)
;; (require 'eaf-music-player)
;; (require 'eaf-camera)
;; (require 'eaf-rss-reader)
;; (require 'eaf-terminal)
;; (require 'eaf-image-viewer)
;; (require 'eaf-vue-demo)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-file-browser)
;; (require 'eaf-mermaid)
;; (require 'eaf-file-manager)
;; (require 'eaf-mindmap)
;; (require 'eaf-video-player)
;; (require 'eaf-org-previewer)
;; (require 'eaf-airshare)
;; (require 'eaf-jupyter)
;; (require 'eaf-netease-cloud-music)
;; (require 'eaf-git)
;; (require 'eaf-system-monitor)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(map! :leader
      (:prefix ("o" . "open")
       :desc "View Calendar" "g" #'calendar))

;; (defun efs/org-babel-tangle-zshrc ()
;;   (when (string-equal (buffer-file-name)
;;                       (expand-file-name "~/.dotfiles/zshrc.org"))
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))

;; (add-hook 'org-mode-hook (λ! (add-hook 'after-save-hook #'efs/org-babel-tangle-zshrc)))
