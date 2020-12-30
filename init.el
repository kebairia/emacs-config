;;; package --- SummarY
;; Load configuration from ~/.config/emacs/settings.org

;;; Commentary:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; Better Defaults
;;----------------
;;(add-to-list
;; 'default-frame-alist '(fullscreen . fullboth)) ; Start emacs/emacsclient in fullscreen mode
(require 'generic-x)                        ; Add syntax high' for ini/conf/batch files
(setq-default
    inhibit-startup-screen t                ; Disable start-up screen
    initial-scratch-message ""              ; Empty the initial *scratch* buffer
    fill-column 80                          ; Set width for automatic line breaks
    tab-width 4                             ; Set width for tabs
    indent-tabs-mode nil                    ; Stop using tabs to indent
    font-lock-maximum-decoration t          ; Supports multiple levels of complexity for highlighting
    ;split-width-threshold nil              ; Disable horizontal window splitting
    sentence-end-double-space nil           ; End a sentence after a dot and a space
    show-trailing-whitespace nil            ; Display trailing whitespaces
    select-enable-clipboard t               ; Merge system's and Emacs' clipboard
    uniquify-buffer-name-style 'forward     ; Uniquify buffer names
    help-window-select t)                   ; Focus new help windows when opened
(setq
    byte-compile-warnings '(cl-functions)   ; disable cl warning
    dired-listing-switches "-ahBGFv1l  --group-directories-first" ;; set dired-listing-switches
    require-final-newline t                 ; Most UNIX tools work best when there’s a
                                            ;   newline on all files.
    tramp-default-method "ssh"              ; set the default tramp method --> ssh
    tab-always-indent 'complete             ; Tab always indent
    initial-major-mode 'org-mode            ; Major mode for scratch buffer
    large-file-warning-threshold 100000000) ; Warn when opening files bigger than 100MB
(menu-bar-mode -1)                          ; Disable menu-bar
(blink-cursor-mode 0)                       ; Disable the cursor blinking
(scroll-bar-mode 0)                         ; Disable the scroll bar "|"
(tool-bar-mode 0)                           ; Disable the tool bar (icons)
(tooltip-mode 0)                            ; Disable the tooltips
(column-number-mode 1)                      ; Show the column number
(show-paren-mode 1)                         ; Show paren mode
(electric-pair-mode 1)
(global-hl-line-mode t)                     ; highlight current line
(fset 'yes-or-no-p 'y-or-n-p)               ; Replace yes/no prompts with y/n
(global-visual-line-mode t)                 ; Enable word-wrapping (fit in the available width of a page)
(put 'narrow-to-region 'disabled nil)       ; enable narrow to region
(global-set-key
    (kbd "TAB") 'tab-to-tab-stop)
(add-hook                                   ; Automatically deletes trailing whitespace
       'write-file-hooks                    ;  after every line when saving a file
       'delete-trailing-whitespace)

(add-hook 'prog-mode-hook #'rainbow-mode)   ; show color faces
(add-hook 'org-mode-hook #'rainbow-mode)    ; for modes(prog,org,txt)
(add-hook 'text-mode-hook #'rainbow-mode)   ;exp #2398f9

;; Fonts, line nubmers ..etc
;;--------------------------
 (set-frame-font "FantasqueSansMono 13" nil t)
 ;; fix the emacsclient font problem
 (add-to-list 'default-frame-alist '(font . "FantasqueSansMono 13")) ;; Font type & size

;;; smart-mode-line-light-theme.el ends here.
 ;; (setq sml/no-confirm-load-theme t)
 ;; (setq sml/theme 'dark)
 ;; (sml/setup)
 (setq-default
     display-line-numbers-current-absolute t ;  Enable the line nubmers
     display-line-numbers-width 2
     display-line-numbers-widen t)
 (add-hook 'text-mode-hook #'display-line-numbers-mode)
 (add-hook 'prog-mode-hook #'display-line-numbers-mode)
 (add-hook 'fundamental-mode-hook #'display-line-numbers-mode)
 (custom-set-faces
   ;; Make Comments/strings italic
   '(font-lock-comment-face ((t (:forground "#928374" :slant italic t))))
   '(font-lock-string-face ((t (:foreground "#b8bb26" :slant italic))))
   '(italic ((t (:underline nil :slant italic))))
   '(dired-directory ((t (:inherit nil :foreground "#00b0d1" :weight bold))))
   '(line-number ((t (:background "#1d2021" :foreground "#7c6f64"))))
   '(line-number-current-line ((t (:background "#504945" :foreground "#fe8019")))))

;; Hist, Backup and auto-save ..etc
;;---------------------------------
(setq backup-directory-alist '(("." . "~/.config/emacs/backup"))
  backup-by-copying t                       ; Don't delink hardlinks
  version-control t                         ; Use version numbers on backups
  delete-old-versions t                     ; Automatically delete excess backups
  kept-new-versions 3                       ; how many of the newest versions to keep
  kept-old-versions 3                       ; and how many of the old
  vc-make-backup-files t                    ; Even version controlled files get to be backed up.
  )
(setq auto-save-file-name-transforms
      '((".*" "~/.config/emacs/undo/" t)))
(setq undo-tree-history-directory-alist     ; Saving persistent tree-undo to a single directory
      '(("." . "~/.config/emacs/undo")))

 (setq is-work nil)                         ; *--=~~ search for explanations ~~=--*
 ;; t means no truncation
 (setq history-length t)
 (setq history-delete-duplicates t)

 (savehist-mode 1)                           ; Saves your minibuffer histories
 (setq
     savehist-file "~/.config/emacs/savehist")    ; Set the savehist file
 (setq savehist-save-minibuffer-history 1)
 (setq savehist-additional-variables         ; Save other histories and other variables as well
     '(kill-ring
       search-ring
       regexp-search-ring))

;; some shortcuts -- files
(global-set-key (kbd "C-c C") (lambda() (interactive)(find-file "~/.config/emacs/init.org")))
(global-set-key (kbd "C-c b") (lambda() (interactive)(find-file "~/org/books.org")))
(global-set-key (kbd "C-c I") (lambda() (interactive)(find-file "~/org/gtd/inbox.org")))
(global-set-key (kbd "C-c L") (lambda() (interactive)(find-file "~/org/links.org")))
(global-set-key (kbd "C-c E") (lambda() (interactive)(find-file "~/org/gtd/emails.org")))
(global-set-key (kbd "<f12>") (lambda() (interactive)(find-file "~/org/files/org.pdf")))
;; Reload buffer with <F5>
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

;; packages
(defun zk/switch-to-agenda ()
     (interactive)
     (org-agenda nil "g"))

(defun zk/split-go-right()
  (interactive)
  (split-window-horizontally)
  (windmove-right))
(defun zk/split-go-down()
  (interactive)
  (split-window-vertically)
  (windmove-down))
 ;; try to go to the other window automaticly
 (global-set-key (kbd "C-x i") 'zk/split-go-right)
 (global-set-key (kbd "C-x m") 'zk/split-go-down)

 ;; Move between buffer
 (global-set-key (kbd "M-n") 'switch-to-next-buffer)
 (global-set-key (kbd "M-p") 'switch-to-prev-buffer)

 ;; Move between Windows
 (global-set-key (kbd "C-x k") 'windmove-up)
 (global-set-key (kbd "C-x j") 'windmove-down)
 (global-set-key (kbd "C-x l") 'windmove-right)
 (global-set-key (kbd "C-x h") 'windmove-left)

 ;; Resize windows
 (global-set-key (kbd "C-M-l") 'shrink-window-horizontally)
 (global-set-key (kbd "C-M-h") 'enlarge-window-horizontally)
 (global-set-key (kbd "C-M-j") 'shrink-window)
 (global-set-key (kbd "C-M-k") 'enlarge-window)

 (global-set-key (kbd "M-o") 'delete-other-windows)
 (global-set-key (kbd "C-x p") 'zk/org-agenda-process-inbox-item)

(use-package gruvbox-theme
 :init )
 ;; the bellow is used so that emacs will trust the elisp code[the theme]
 ;; in future
(custom-set-variables
 '(custom-enabled-themes  '(gruvbox-dark-hard))
 '(custom-safe-themes
    '("4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb"
       default)))

(setq evil-want-keybinding nil)                   ;; this statement is required to enable evil/evil-colleciton mode
(evil-mode 1)                                     ;; enable evil-mode
(setq evil-want-abbrev-expand-on-insert-exit nil)
(use-package evil-collection                      ;; evil-friendly binding for many modes
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; (use-package evil-magit
;;   :after evil)
(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(setq                                             ;;automatically use evil for ibuffer and dired
 evil-emacs-state-modes
    (delq 'ibuffer-mode evil-emacs-state-modes))

(use-package evil-leader
;; needs to be enabled before M-x evil-mode!
    :ensure t
    :config
        (evil-leader/set-leader ",")
        (evil-leader/set-key
         "a" 'zk/switch-to-agenda
         "w" 'org-agenda-week-view
         "m" 'org-agenda-month-view
         "d" 'deft
         "I" 'zetteldeft-insert-list-links
         "N" 'zetteldeft-new-file-and-link
         "B" 'zetteldeft-new-file-and-backlink
         "f" 'pdf-links-action-perform
         "b" 'ibuffer
         "t" 'term
         "c" 'org-capture
         ;; "g" 'magit-status
         "r" 'bookmark-bmenu-list
         "l" 'org-store-link
         "L" 'org-insert-link
         "n" 'org-noter
         "q" 'kill-current-buffer)
        (evil-leader-mode 1)
        (global-evil-leader-mode 1))
         ;;"B" 'zetteldeft-backlink-add
         ;;"s" 'zk/gen-scratch-buffer

;;Use minions to hide all minor modes
(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

;; disable linum-mode
(add-hook 'ibuffer-mode (lambda() (linum-mode -1)))
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Use Ibuffer for Buffer List
;; create a function that define a group
(setq ibuffer-saved-filter-groups
    '(("default"
       ("Emacs"  (or
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*scratch\\*$")
       ))
       ("Agenda"  (or
                   (name . "inbox.org")
                   (name . "next.org")
                   (name . "someday.org")
                   (name . "emails.org")
                   (name . "archive.org")
                   (name . "habits.org")
                   (name . "projects.org")
                   (name . "weekly_reviews.org")
           ))

       ("Org"  (name . "^.*org$"))
       ("PDF"  (name . "^.*pdf"))
       ("Python"  (name . "^.*py$"))
       ("Elisp"  (name . "^.*el"))
       ("Web"  (or
                   (name . "^.*html$")
                   (name . "^.*css")
                   (name . "^.*php")
           ))
       ("Dired"  (mode . dired-mode))
     ))
  )

(add-hook 'ibuffer-mode-hook
 '(lambda ()
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default"))) ;; use the group default

(use-package which-key
 :ensure t
 :config
 (which-key-mode))

;; it looks like counsel is a requirement for swiper
;; counsel give us a nice looking interface when we use M-x
(use-package counsel
  :ensure t)

(global-set-key (kbd "C-x o") 'ace-window)

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key "\M-S" 'counsel-org-goto)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-src-window-setup 'current-window)
  :custom-face
   (org-block ((t (:background "#181818"))))
   (org-done ((t (:strike-through t :weight bold))))
   (org-headline-done ((t (:strike-through t))))
   (org-document-title ((t (:foreground "#00b0d1" :weight bold :height 1.1 ))))
   (org-document-info ((t (:foreground "#00b0d1"  :height 1.0 ))))
   (org-document-info-keyword ((t (:foreground "#928374" :slant italic :height 1.0 ))))
   (org-level-1 ((t (:foreground "#00b0d1" :weight bold))))
   (org-level-2 ((t (:weight bold))))
   (org-level-3 ((t (:weight bold))))
   (org-level-4 ((t (:foreground "tomato" :weight bold))))
   (org-level-5 ((t (:weight bold))))
   (org-level-6 ((t (:weight bold))))
   (org-level-7 ((t (:weight bold))))
   (font-lock-comment-face ((t (:forground "#928374" :slant italic t))))
   (org-date-selected ((t (:foreground "#00b0d1" :weight bold :height 1.1 :underline t))))
   (org-done ((t (:foreground "#16a637" :strike-through t :weight bold))))
   (org-headline-done ((t (:foreground "#16a637" :strike-through t ))))
   (org-date ((t (:foreground "#458588" :underline t))))
   ;;(org-link ((t (:foreground "SpringGreen3" :underline t)))))
   (org-link ((t (:foreground "SeaGreen3" :underline t)))))
(setq org-fontify-done-headline t
      org-hide-leading-stars t
      org-pretty-entities t
      org-image-actual-width nil
      org-hide-emphasis-markers t
      org-log-into-drawer t
      org-log-done '(time)
      org-log-reschedule '(time)
      org-babel-min-lines-for-block-output 0
      org-deadline-warning-days 0
      org-emphasis-alist
         '(("*" (bold ))
            ("/" italic)
            ("_" underline)
            ("=" (:foreground "brown2" :weight bold))
            ("~" (:foreground "#928374" :slant italic))
            ("+" (:strike-through t))))
;; When editing a code snippet, use the current window rather than popping open a new one
(setq org-src-window-setup 'current-window)
;;(add-hook 'org-mode-hook 'org-indent-mode)
;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key 'expert)
;; use '⌄/⌵⤵' instead of '...' in headlines
(setq org-ellipsis "⤵")
;; use '•' instead of '-' in lists
(font-lock-add-keywords 'org-mode
                   '(("^ *\\([-]\\) "
                      (0 (prog1 ()
                           (compose-region
                            (match-beginning 1)
                            (match-end 1) "•"))))))

(setq org-agenda-directory "~/org/gtd/"
      org-agenda-files '("~/org/gtd" ))                 ;; org-agenda-files
(setq
    org-agenda-start-on-weekday 0                       ;; Weekday start on Sunday
     org-treat-S-cursor-todo-selection-as-state-change nil;; S-R,S-L skip the note/log info[used when fixing the state]
      org-agenda-tags-column -140                     ;; Set tags far to the right
      org-clock-out-remove-zero-time-clocks t         ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      org-clock-persist t                             ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      org-use-fast-todo-selection t                   ;; from any todo state to any other state; using it keys
     org-agenda-window-setup 'only-window)              ;; Always open my agenda in fullscreen

(setq org-agenda-prefix-format
  '((agenda . " %i %-12:c%?-12t% s")
    (todo   . " ")
    (tags   . " %i %-12:c")
    (search . " %i %-12:c")))

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
(setq org-log-done 'time)

 (add-hook 'org-agenda-mode-hook                        ;; disable line-number when i open org-agenda view
           (lambda() (display-line-numbers-mode -1)))

;; (define-key global-map (kbd "C-c c") 'org-capture)
;; (define-key global-map (kbd "C-c a") 'org-agenda)

(setq org-agenda-block-separator 9472)     ; use 'straight line' as a block-agenda divider
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                ((org-agenda-overriding-header "Tasks")
                 (org-agenda-span 'day)
                 (org-agenda-prefix-format "  %i %-12:c (%e) ")

                   (org-agenda-files '("~/org/gtd/next.org"))
                   (org-deadline-warning-days 0)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                         ))

          (agenda nil
                  ((org-agenda-overriding-header "Deadlines")
                   (org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-prefix-format "  %?-12t% s")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))))

          (tags-todo "inbox"
                     ((org-agenda-overriding-header "Inbox")
                      (org-agenda-files '("~/org/gtd/inbox.org"))
                      (org-agenda-prefix-format "  %?-12t% s")))

          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "Completed today")
                 (org-agenda-prefix-format "  %?-12t% s")
            ))))))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 48)
;;(setq org-habit-show-habits-only-for-today t)

;; Refiling [need reading]
(setq org-refile-use-outline-path 'file
 org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("~/org/gtd/next.org" :level . 0)
                           ("~/org/links.org" :level . 1)
                           ("~/org/ideas.org" :level . 1)
                           ("someday.org" :level . 0)
                           ("~/org/gtd/projects.org" :maxlevel . 2)))

(setq org-capture-templates
   `(("i" "Inbox" entry  (file "~/org/gtd/inbox.org")
    ,(concat "* TODO %?\n"
             "/Entered on/ %U"))))

;; PS: check out the original code from here:
;; https://github.com/gjstein/emacs.d/blob/master/config/gs-org.el

;;clocking-out changes NEXT to HOLD
;;clocking-in changes HOLD to NEXT
(setq org-clock-in-switch-to-state 'zk/clock-in-to-next)
(setq org-clock-out-switch-to-state 'zk/clock-out-to-hold)
(defun zk/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
   Skips capture tasks, projects, and subprojects.
   Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO")))
      "NEXT")
     ((and (member (org-get-todo-state) (list "HOLD")))
      "NEXT")
      )))
(defun zk/clock-out-to-hold (kw)
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "NEXT")))  "HOLD")
      )))

(setq org-todo-keyword-faces
 '(
   ("TODO" . (:foreground "brown2" :weight bold))
   ("READ" . (:foreground "brown2" :weight bold))

   ("NEXT" . (:foreground "#00b0d1"  :weight bold ))
   ("READING" . (:foreground "#00b0d1"  :weight bold ))

   ("DONE" . (:foreground "#16a637" :weight bold))

   ("HOLD" . (:foreground "orange"  :weight bold))

   ("CANCELED" . (:foreground "gray" :background "red1" :weight bold))
 ))
