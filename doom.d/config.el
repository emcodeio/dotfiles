(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/doom.d/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (λ! (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

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
    )
(add-hook! 'after-init-hook #'openwith-mode)

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

(after! org
  (add-hook 'org-mode-hook (λ! (org-bullets-mode 1)))
  (add-hook 'org-mode-hook
      (λ!
        (setq syntax-propertize-function 'org-mode-<>-syntax-fix)
        (syntax-propertize (point-max))))
  (setq org-directory "~/Org/"
        org-agenda-files '("~/Org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-journal-dir "~/Org/journal/"
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"
        org-hide-emphasis-markers t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "BLOG(b)"           ; Blog writing assignments
             "GYM(g)"            ; Things to accomplish at the gym
             "PROJ(p)"           ; A project that contains other tasks
             "VIDEO(v)"          ; Video assignments
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled

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

(map! :map gradle-mode-map
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

(defun efs/org-babel-tangle-zshrc ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/zshrc.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (λ! (add-hook 'after-save-hook #'efs/org-babel-tangle-zshrc)))
