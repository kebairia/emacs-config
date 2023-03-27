(defvar my/init-start-time (current-time) "Time when init.el was started")
(defvar my/section-start-time (current-time) "Time when section was started")

(setq
 site-run-file nil                         ; No site-wide run-time initializations. 
 inhibit-default-init t                    ; No site-wide default library
 gc-cons-threshold most-positive-fixnum    ; Very large threshold for garbage
                                           ; collector during init
 package-enable-at-startup nil)            ; We'll use straight.el

(setq native-comp-eln-load-path
      (list (expand-file-name "eln-cache" user-emacs-directory)))

;; Reset garbage collector limit after init process has ended (8Mo)
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))

(setq straight-check-for-modifications nil)
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
  (load bootstrap-file nil 'nomessage))

(setq package-list
      '(evil                                 ;; an extensible vi layer for Emacs
        evil-collection
        evil-org                             ;; evil-leader is a dependency for evil-org (as I see)
        evil-org-mode                        ;; Use evil with org and org-agenda
        org
        org-appear                           ;; show marker emphasis when moving accross the word
        org-cliplink                         ;; see if we need that 
        magit
        ibuffer
        which-key
        ; Completion
        selectrum
        ctrlf
        corfu
        cape
        eglot
        aggressive-indent
        indent-guide
        use-package
        pdf-tools
        org-pdfview
        org-noter
        ;; Notes
        org-roam                               ; Roam implementation for Emacs/org
        org-roam-ui                            ; A graph generator for org-roam notes
        deft                                   ; to list your notes
        citar                                  ; A better ui completion for your bibliography
        ;; IDE
        ;;undo-redo                            ;; for Emacs 28
        indent-guide
        ;; MODES
        yaml-mode
        jinja2-mode
        hcl-mode
        terraform-mode
        ;; LATEX
        engrave-faces
        ;; news
        elfeed
        elfeed-score
        ;;elfeed-tube
        ;;elfeed-tube-mpv
        ;;mpv
        )
      )
;; loop over the list above and install all the packages
(dolist (package package-list)
  (straight-use-package package))



;; Special case for pdf-tools that has recently (2022) changed maintainer
(straight-use-package
 '(pdf-tools :type git :host github :repo "vedang/pdf-tools"))
;; Glossary, Acronyms, and Index capability within Org
(straight-use-package
 '(org-glossary :type git :host github :repo "tecosaur/org-glossary"))
(straight-use-package
 '(elfeed-tube :type git :host github :repo "karthink/elfeed-tube"))

;; NANO theme
(straight-use-package
 '(nano-theme :type git :host github :repo "rougier/nano-theme"))
;; NANO modeline
(straight-use-package
 '(nano-modeline :type git :host github :repo "rougier/nano-modeline"))
;; SVG tags, progress bars & icons
(straight-use-package
 '(svg-lib :type git :host github :repo "rougier/svg-lib"))

;; Replace keywords with SVG tags
(straight-use-package
 '(svg-tag-mode :type git :host github :repo "rougier/svg-tag-mode"))

(setq-default
 inhibit-startup-screen t               ; Disable start-up screen
 inhibit-startup-message t              ; Disable startup message
 inhibit-startup-echo-area-message t    ; Disable initial echo message
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 initial-buffer-choice t)               ; Open *scratch* buffer at init

(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-default-coding-systems 'utf-8)     ; Set default value of various coding systems
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

(setq auto-save-list-file-prefix ; Prefix for generating auto-save-list-file-name
      (expand-file-name ".auto-save-list/.saves-" user-emacs-directory)
      auto-save-default t        ; Auto-save every buffer that visits a file
      auto-save-timeout 20       ; Number of seconds between auto-save
      auto-save-interval 200)    ; Number of keystrokes between auto-saves

(setq backup-directory-alist       ; File name patterns and backup directory names.
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      make-backup-files t          ; Backup of a file the first time it is saved.
      vc-make-backup-files nil     ; No backup of files under version contr
      backup-by-copying t          ; Don't clobber symlinks
      version-control t            ; Version numbers for backup files
      delete-old-versions t        ; Delete excess backup files silently
      kept-old-versions 6          ; Number of old versions to keep
      kept-new-versions 9          ; Number of new versions to keep
      delete-by-moving-to-trash t) ; Delete files to trash

(setq bookmark-default-file (expand-file-name "bookmark" user-emacs-directory))

(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(require 'savehist)

(setq kill-ring-max 50
      history-length 50)

(setq savehist-additional-variables
      '(kill-ring
        command-history
        set-variable-value-history
        custom-variable-history   
        query-replace-history     
        read-expression-history   
        minibuffer-history        
        read-char-history         
        face-name-history         
        bookmark-history
        file-name-history))

(put 'minibuffer-history         'history-length 50)
(put 'file-name-history          'history-length 50)
(put 'set-variable-value-history 'history-length 25)
(put 'custom-variable-history    'history-length 25)
(put 'query-replace-history      'history-length 25)
(put 'read-expression-history    'history-length 25)
(put 'read-char-history          'history-length 25)
(put 'face-name-history          'history-length 25)
(put 'bookmark-history           'history-length 25)

(setq history-delete-duplicates t)

(let (message-log-max)
  (savehist-mode))

(require 'server)

(unless (server-running-p)
  (server-start))

(defun zk/split-go-right()
  (interactive)
  (split-window-horizontally)
  (windmove-right))
(defun zk/split-go-down()
  (interactive)
  (split-window-vertically)
  (windmove-down))

(bind-key "C-c C" (lambda() (interactive)(find-file "~/.config/emacs/init.org")))
(bind-key "C-c b" (lambda() (interactive)(find-file "~/org/books.org")))

(bind-key "M-n" 'switch-to-next-buffer)
(bind-key "M-p" 'switch-to-prev-buffer)

(bind-key "C-c k" 'window-up)
(bind-key "C-c j" 'window-down)
(bind-key "C-c l" 'window-right)
(bind-key "C-c h" 'window-left)

(bind-key "C-c i" 'zk/split-go-right)
(bind-key "C-c m" 'zk/split-go-down)

(bind-key "C-c c" 'org-capture)

(setq my/section-start-time (current-time))

(defun my/make-frame ()
"Create a new frame and switch to *scratch* buffer."

(interactive)
(select-frame (make-frame))
(switch-to-buffer "*scratch*"))

(defun my/kill-emacs ()
"Delete frame or kill Emacs if there is only one frame."

(interactive)
(condition-case nil
    (delete-frame)
(error (save-buffers-kill-terminal))))

(require 'frame)

;; Default frame settings
(setq default-frame-alist '((min-height . 1)  '(height . 45)
                        (min-width  . 1)  '(width  . 81)
                        (vertical-scroll-bars . nil)
                        (internal-border-width . 24)
                        (left-fringe . 0)
                        (right-fringe . 0)
                        (tool-bar-lines . 0)
                        (menu-bar-lines . 0)))

;; Default frame settings
(setq initial-frame-alist default-frame-alist)

(bind-key "M-n"        #'my/make-frame)
(bind-key "C-x k"    #'kill-this-buffer)
(bind-key "C-c s"      #'nano-new-frame)
(bind-key "M-`"        #'other-frame)
(bind-key "C-z"        nil)
(bind-key "<M-return>" #'toggle-frame-maximized)

(with-eval-after-load 'org
(bind-key "<M-return>" #'toggle-frame-maximized 'org-mode-map))

(setq-default window-divider-default-right-width 24
            window-divider-default-places 'right-only
            left-margin-width 0
            right-margin-width 0
            window-combination-resize nil) ; Do not resize windows proportionally

(window-divider-mode 1)

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse
    uniquify-separator " • "
    uniquify-after-kill-buffer-p t
    uniquify-ignore-buffers-re "^\\*")

(setq display-line-numbers 'relative)    ; Enable relative number
(setq-default
 display-line-numbers-current-absolute t ; Enable the line nubmers
 display-line-numbers-width 2
 display-line-numbers-widen t)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'fundamental-mode-hook #'display-line-numbers-mode)

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
         ("Lisp"  (name . "^.*el"))
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

(setq vc-follow-symlinks t)

(setq-default show-help-function nil    ; No help text
            use-file-dialog nil       ; No file dialog
            use-dialog-box nil        ; No dialog box
            pop-up-windows nil)       ; No popup windows

;; (tooltip-mode -1)                       ; No tooltips
(scroll-bar-mode -1)                    ; No scroll bars
(tool-bar-mode -1)                      ; No toolbar
(menu-bar-mode 1)                       ; No menu bar

(require 'which-key)
(which-key-mode)

;; put this before loading evil to work
(setq evil-want-C-i-jump nil)
;; ;; this statement is required to enable evil/evil-colleciton mode
(setq evil-want-abbrev-expand-on-insert-exit nil)
(setq evil-want-keybinding nil)
(evil-mode 1)
(when (require 'evil-collection nil t)
(evil-collection-init))

(setq-default cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
            cursor-type '(hbar . 2)            ; Underline-shaped cursor
            cursor-intangible-mode t           ; Enforce cursor intangibility
            x-stretch-cursor nil)              ; Don't stretch cursor to the glyph width

(blink-cursor-mode 0)                            ; Still cursor

(setq-default use-short-answers t                     ; Replace yes/no prompts with y/n
            confirm-nonexistent-file-or-buffer nil) ; Ok to visit non existent files

(delete-selection-mode 1)

(setq-default visible-bell nil             ; No visual bell      
            ring-bell-function 'ignore)  ; No bell

(setq-default scroll-conservatively 101       ; Avoid recentering when scrolling far
            scroll-margin 2                 ; Add a margin when scrolling vertically
            recenter-positions '(5 bottom)) ; Set re-centering positions

(setq-default select-enable-clipboard t) ; Merge system's and Emacs' clipboard

(setq help-window-select t)             ; Focus new help windows when opened

(bind-key "C-h f"   #'helpful-callable) ; Look up callable
(bind-key "C-h v"   #'helpful-variable) ; Look up variable
(bind-key "C-h k"   #'helpful-key)      ; Look up key 
(bind-key "C-c C-d" #'helpful-at-point) ; Look up the current symbol at point
(bind-key "C-h F"   #'helpful-function) ; Look up *F*unctions (excludes macros).
(bind-key "C-h C"   #'helpful-command)  ; Look up *C*ommands.

(setq my/section-start-time (current-time))

(require 'nano-theme)
;; (setq nano-fonts-use t) ; Use theme font stack
(nano-modeline-mode)    ; Use nano-modeline
(nano-mode)  
(nano-dark)             ; Use theme dark version
(setq nano-font-family-monospaced "FantasqueSansMono")
(setq nano-font-family-proportional nil)
(setq nano-font-size 17)

(setq-default fill-column 80                          ; Default line width 
              sentence-end-double-space nil           ; Use a single space after dots
              bidi-paragraph-direction 'left-to-right ; Faster
              truncate-string-ellipsis "…")

(require 'nano-theme)

;; Nicer glyphs for continuation and wrap 
(set-display-table-slot standard-display-table
                        'truncation (make-glyph-code ?… 'nano-faded))

(defface wrap-symbol-face
  '((t (:family "Fira Code"
        :inherit nano-faded)))
  "Specific face for wrap symbol")

(set-display-table-slot standard-display-table
                        'wrap (make-glyph-code ?↩ 'wrap-symbol-face))

(setq my/section-start-time (current-time))

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)

(setq-default indent-tabs-mode nil        ; Stop using tabs to indent
              tab-always-indent 'complete ; Indent first then try completions
              tab-width 4)                ; Smaller width for tab characters

;; Let Emacs guess Python indent silently
(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

(require 'paren)
;; (setq show-paren-style 'expression)
(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-when-point-inside-paren nil)
(show-paren-mode)

(electric-pair-mode 1)

(require 'hl-line)

(global-hl-line-mode)

(indent-guide-mode t)
(setq indent-guide-char "|")

(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)

(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))) ; set the pdf-view incompatible-modes[linum mode: line numbers]

(require 'org-noter)
(bind-key "C-c n n" #'org-noter)

(setq org-noter-auto-save-last-location t
      org-noter-doc-split-fraction (quote (0.7 . 0.7))
      org-noter-notes-window-behavior nil
      org-noter-notes-window-location "Vertical"
      org-noter-always-create-frame nil
      org-noter-separate-notes-from-heading t)

(selectrum-mode +1)                         ; enable selectrum mode
(straight-use-package 'selectrum-prescient) ; to make sorting and filtering more intelligent
(selectrum-prescient-mode +1)               ; same
;; to save your command history on disk, so the sorting gets more
;; intelligent over time
(prescient-persist-mode 1)

(setq ctrlf-default-search-style 'fuzzy-regexp)
(setq ctrlf-auto-recenter 1)
(setq ctrlf-highlight-line 1)

(ctrlf-mode +1)

(require 'corfu)

(setq corfu-cycle t                ; Enable cycling for `corfu-next/previous'
      corfu-auto t                 ; Enable auto completion
      corfu-auto-delay 60.0        ; Delay before auto-completion shows up
      corfu-separator ?\s          ; Orderless field separator
      corfu-quit-at-boundary nil   ; Never quit at completion boundary
      corfu-quit-no-match t        ; Quit when no match
      corfu-preview-current nil    ; Disable current candidate preview
      corfu-preselect-first nil    ; Disable candidate preselection
      corfu-on-exact-match nil     ; Configure handling of exact matches
      corfu-echo-documentation nil ; Disable documentation in the echo area
      corfu-scroll-margin 5)       ; Use scroll margin

(global-corfu-mode)

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; completion-at-point is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Completion in source blocks
(require 'cape)

(add-to-list 'completion-at-point-functions 'cape-symbol)

(require 'cape)
(global-corfu-mode)
;; (setq completion-at-point-functions 'cape)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

(setq-default org-directory "~/org"
              org-ellipsis " …"              ; Nicer ellipsis
              org-tags-column 1              ; Tags next to header title
              org-hide-emphasis-markers t    ; Hide markers
              org-cycle-separator-lines 2    ; Number of empty lines between sections
              org-use-tag-inheritance nil    ; Tags ARE NOT inherited 
              org-use-property-inheritance t ; Properties ARE inherited
              org-indent-indentation-per-level 2 ; Indentation per level
              org-link-use-indirect-buffer-for-internals t ; Indirect buffer for internal links
              org-fontify-quote-and-verse-blocks t ; Specific face for quote and verse blocks
              org-return-follows-link nil    ; Follow links when hitting return
              org-image-actual-width nil     ; Resize image to window width
              org-indirect-buffer-display 'other-window ; Tab on a task expand it in a new window
              org-outline-path-complete-in-steps nil ; No steps in path display
              org-log-into-drawer t)         ; Log into drawers

(setq my/section-start-time (current-time))

(setq org-latex-create-formula-image-program 'dvisvgm)

(setq org-ellipsis " ⤵")
;; use '•' instead of '-' in lists
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 ()
                                (compose-region
                                 (match-beginning 1)
                                 (match-end 1) "•"))))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "CANCELED")))
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

(require 'org-glossary)

(setq org-appear-autolinks t
      org-appear-autosubmarkers t)
(add-hook 'org-mode-hook (lambda () (org-appear-mode 1)))

(setq org-capture-templates
      `(("i" " inbox" entry  (file "~/org/gtd/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))

        ("p" " post" entry  (file "~/org/posts.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))

        ("L" " link" entry (file+headline "~/org/gtd/inbox.org" "Links")
         ,(concat "* TODO %a %?\n"
                  "/Entered on/ %U") :immediate-finish t)

        ("s" " slipbox" entry (file "~/dox/braindump/org-files/fleetnotes.org")
         "* %<%a, %d %b %y (%H:%M)> : %?\n")

        ;; ("e" " email" entry (file+headline "~/org/gtd/emails.org" "Emails")
        ;;  "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")

        ;; ("m" "mood" entry (file "~/org/mood.org" )
        ;;  ,(concat "* %? \n %^{MOOD} \n"
        ;;           "/Entered on/ %U") :immediate-finish t)
        ))

(require 'org-protocol)

(defun zk/switch-to-agenda ()
  (interactive)
  (org-agenda nil "g"))

(bind-key "C-c a" #'zk/switch-to-agenda)

(require 'evil-org)
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(setq org-agenda-directory "~/org/gtd/"
      org-agenda-files '("~/org/gtd" ))                  ;; org-agenda-files

(setq org-agenda-dim-blocked-tasks nil                    ;; Do not dim blocked tasks
      org-agenda-span 'day                                ;; show me one day
      org-agenda-inhibit-startup t                        ;; Stop preparing agenda buffers on startup:
      org-agenda-use-tag-inheritance nil                  ;; Disable tag inheritance for agendas:
      org-agenda-show-log t
      org-agenda-skip-scheduled-if-deadline-is-shown t     ;; skip scheduled if they are already shown as a deadline
      org-agenda-deadline-leaders '("!D!: " "D%2d: " "")
      org-agenda-scheduled-leaders '("" "S%3d: "))


(setq org-agenda-start-on-weekday 0                          ;; Weekday start on Sunday
      org-treat-S-cursor-todo-selection-as-state-change nil ;; S-R,S-L skip the note/log info[used when fixing the state]
      org-log-done 'time
      org-agenda-tags-column -130                          ;; Set tags far to the right
      org-clock-out-remove-zero-time-clocks t              ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      org-clock-persist t                                  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      org-use-fast-todo-selection t                        ;; from any todo state to any other state; using it keys
      org-agenda-window-setup 'only-window)                 ;; Always open my agenda in fullscreen

;; define org's states
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
;; sort my org-agenda preview

;;Thanks to Erik Anderson, we can also add a hook that will log when we activate
;;a task by creating an “ACTIVATED” property the first time the task enters the NEXT state:
(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
(add-hook 'org-agenda-mode-hook                            ;; disable line-number when i open org-agenda view
          (lambda() (display-line-numbers-mode -1)))

;; (define-key global-map (kbd "C-c a") 'org-agenda)

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t %s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

(setq org-agenda-time-grid
      '((daily today require-timed)
        ()
        "......" "----------------"))

(setq org-agenda-current-time-string "   now")
;; (setq org-agenda-time-grid
;;       '((daily today require-timed)
;;         (800 1000 1200 1400 1600 1800 2000)
;;         "......" "----------------"))

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up scheduled-down
                priority-down category-keep deadline-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

(setq org-agenda-block-separator  9472)                  ;; use 'straight line' as a block-agenda divider
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-deadline-warning-days 365)))

          (todo "NEXT"
                ((org-agenda-overriding-header "In Progress")
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-files '("~/org/gtd/someday.org"
                                     "~/org/gtd/projects.org"
                                     "~/org/gtd/next.org"))
                 ))
          (todo "TODO"
                ((org-agenda-overriding-header "inbox")
                 (org-agenda-files '("~/org/gtd/inbox.org"))))

          (todo "TODO"
                ((org-agenda-overriding-header "Emails")
                 (org-agenda-files '("~/org/gtd/emails.org"))))

          (todo "TODO"
                ((org-agenda-overriding-header "Projects")
                 (org-agenda-files '("~/org/gtd/projects.org")))
                )

          (todo "TODO"
                ((org-agenda-overriding-header "One-off Tasks")
                 (org-agenda-files '("~/org/gtd/next.org"))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if
                                             'deadline 'scheduled))))
          nil))))



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

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 48)
(setq org-habit-show-habits-only-for-today t)

;; Refiling [need reading]
;;tell org-mode we want to specify a refile target using the file path.
(setq org-refile-use-outline-path 'file
 org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("~/org/gtd/next.org" :level . 0)
                           ("~/org/ideas.org" :level . 1)
                           ("~/org/links.org" :level . 1)
                           ("~/org/gtd/someday.org" :regexp . "\\(?:\\(?:Task\\|idea\\|p\\(?:\\(?:os\\|rojec\\)t\\)\\)s\\)")
                           ("projects.org" :regexp . "\\(?:Tasks\\)"))) 
;;("someday.org" :level . 0)

;; (require 'org-tempo)
;; (add-to-list 'org-structure-template-alist
;;              '("S" . "src emacs-lisp"
;;                "P" . "src python"
;;                ))

(setq-default org-src-fontify-natively t         ; Fontify code in code blocks.
              org-adapt-indentation nil          ; Adaptive indentation
              org-src-tab-acts-natively t        ; Tab acts as in source editing
              org-confirm-babel-evaluate nil     ; No confirmation before executing code
              org-edit-src-content-indentation 0 ; No relative indentation for code blocks
              org-fontify-whole-block-delimiter-line t) ; Fontify whole block

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)
   (R . t)
   ))

(bind-key "C-c g" #'magit)
(advice-add 'magit-set-header-line-format :override #'ignore)

(setq org-roam-directory (file-truename "/home/zakaria/dox/braindump/org-files"))
(org-roam-db-autosync-mode)                    ; autosync for db
;; (setq org-roam-dailies-directory (file-truename "/home/zakaria/org/daily")) ; directory for my dailies
(setq org-roam-db-gc-threshold most-positive-fixnum) ; Garbage collection
; Keybindings
(bind-key "C-c n f" #'org-roam-node-find)
(bind-key "C-c n l" #'org-roam-buffer-toggle)
(bind-key "C-c n g" #'org-roam-ui-mode)
(bind-key "C-c n i" #'org-roam-node-insert)
(bind-key "C-c n t" #'org-roam-tag-add)
(bind-key "C-c n r" #'org-roam-ref-add)
(bind-key "C-c n c" #'org-roam-capture)
(bind-key "C-c n j" #'org-roam-dailies-capture-today)
(bind-key "C-c n d" #'org-roam-dailies-map)
;; (require 'org-roam-protocol))

;;Configuring the Org-roam buffer display
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.45)
               (window-height . fit-window-to-buffer)))

;; org-roam templates
(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new
         (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?\n* References :ignore:\n#+print_bibliography"
         :if-new
         (file+head "refs/%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("c" "comp-sci" plain "%?"
         :if-new
         (file+head "cs/%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n#+filetags: :computer-science:\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))

;; (cl-defmethod org-roam-node-type ((node org-roam-node))
;;   "Return the TYPE of NODE."
;;   (condition-case nil
;;       (file-name-nondirectory
;;        (directory-file-name
;;         (file-name-directory
;;          (file-relative-name (org-roam-node-file node) org-roam-directory))))
;;     (error "")))
;; TODO: use icons instead
;; (setq org-roam-node-display-template
;;       (concat "${type:10} ${title:*} " (propertize "${tags:20}" 'face 'org-tag)))
(setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

(setq org-roam-ui-sync-theme nil
      org-roam-ui-follow t
      org-roam-ui-update-on-save t
      org-roam-ui-open-on-start t)

;; disable linum-mode (line number)
(add-hook 'deft
          '(lambda () (linum-mode nil)))
(use-package deft
  :commands (deft)
  :custom       (deft-directory "~/org/notes" )
  (deft-recursive t)
  (deft-extensions '("org" "md" "txt") )
  (deft-use-filename-as-title t)
  (deft-file-naming-rules
    '((noslash . "-")
      (nospace . "-")
      (case-fn . downcase))
    deft-org-mode-title-prefix t
    deft-text-mode 'org-mode))

(cl-defmethod org-roam-node-directories ((node org-roam-node))
  (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (format "%s" (car (split-string dirs "/")))
    ""))

(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
                                :from links
                                :where (= dest $s1)
                                :and (= type "id")]
                       (org-roam-node-id node)))))
    (format "[%d]" count)))


(setq org-roam-node-display-template
      (concat "${directories:10} ${title:*} ${backlinkscount:6}" (propertize "${tags:20}" 'face 'org-tag) ))

(require 'engrave-faces)

(setq blog-root-dir "~/dox/blog"
      blog-path "~/dox/blog/content/"
      blog-port "8080")


;; (defun zk/create-blog(filename)
;;   (interactive)
;;   (find-file (concat blog-path 'filename)))

;; I took `capitalize-first-char` function from this function is from
;; https://emacs.stackexchange.com/a/12614

(defun zk/capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

;; Refine the post filename, remove spaces and subtitue that with '-'
(defun zk/refine-post-filename(string)
  "Remove space from STRING"
  (downcase (replace-regexp-in-string " " "-" string)))

(defun zk/create-post (&optional _post)
  "Function for creating post
               Prompt me for the post name, if it exist, warn me.
               else write the post using the date as a prefix and use `.org' as an extension 
               and then put some org keywords "
  (interactive)
  (setq _post (read-string " Post: ")
        _date (format-time-string "%Y-%m-%d" (current-time))
        _ext ".org"
        _path "/home/zakaria/dox/blog/content/")

  ;; concatenate all variables together, and subtitue all whitespaces with  `-'
  ;; and downcase the name
  (setq filename
        (zk/refine-post-filename (concat  _path _date "-" _post _ext)))

  (if (file-exists-p filename)
      (message (concat  "File " (concat "'" _post "'" " already exists")))
    (switch-to-buffer (find-file filename )))

  ;; setting variables
  (setq _title (zk/capitalize-first-char _post)
        _author "Zakaria.K"
        _email (message-user-mail-address)
        _date (format-time-string "%d %b %Y %a")
        _other "#+OPTIONS: html5-fancy: t\n#+begin_date\n{{{date}}}\n#+end_date\n")

  ;; insert template
  (insert (format "#+TITLE: %s\n#+SUBTITLE: \n#+AUTHOR: %s \n#+EMAIL: %s \n#+DATE: %s \n#+KEYWORDS: \n%s"
                  _title
                  _author
                  _email
                  _date
                  _other
                  ))
  ;; And then start the local server
  (zk/start-blog)
  )

;; Keybinding for the function
(global-set-key (kbd "C-c P") 'zk/create-post)

(defun zk/start-blog()
  (interactive)
  (setq httpd-root "~/dox/blog/public"
        httpd-port blog-port)
  (httpd-start))

(setq-default shell-file-name          "/bin/bash"
              explicit-shell-file-name "/bin/bash")
