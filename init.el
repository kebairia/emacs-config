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

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;- Clipboard stuff ---------------------
(setq select-active-regions nil            ; avoids saving active regions to the primary selection entirely.
    select-enable-primary t                ; killing/yanking interacting with primary X11 selection
    select-enable-clipboard t              ; makes killing/yanking interact with clipboard X11 selection
    mouse-drag-copy-region t               ; selection with a mouse being immediately injected to the kill ring
    save-interprogram-paste-before-kill t );Save the current (system) clipboard content before replacing it with the Emacs’ text.

;; Better Defaults
;;----------------
;; Birthday Present
(when (string= "03-14" (format-time-string "%m-%d"))
      (animate-birthday-present user-full-name))
;;(add-to-list
;; 'default-frame-alist '(fullscreen . fullboth)) ; Start emacs/emacsclient in fullscreen mode
(require 'generic-x)                        ; Add syntax high' for ini/conf/batch files
(setq-default
    inhibit-startup-screen t                ; Disable start-up screen
    initial-scratch-message ""              ; Empty the initial *scratch* buffer
    fill-column 80                          ; Set width for automatic line breaks
    evil-want-C-i-jump nil                  ; enable the TAB key in emacs-terminal
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
    scroll-margin 3
    scroll-conservatively 100000
    scroll-preserve-screen-position 1
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
 (setq sml/no-confirm-load-theme t)
 (setq sml/theme 'dark)
 (sml/setup)
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

;; Prettify Symbols
;;-----------------
 (setq-default prettify-symbols-alist '(
         ("-->" . "→")
         ("->>" . "↠")
         ("==>" . "⇒")
         ("/=" . "≠")
         ("<=" . "≤")
         (">=" . "≥")
         ("=<<" . "=≪")
         (">>=" . "≫=")
         ("<=<" . "↢")
         (">=>" . "↣")
         ("alpha" . "α")
         ("beta" . "β")
         ("gamma" . "γ")
         ("delta" . "δ")
         ("epsilon" . "ε")
         ("zeta" . "ζ")
         ("eta" . "η")
         ("theta" . "θ")
         ("iota" . "ι")
         ("kappa" . "κ")
         ("lambda" . "λ")
         ("bigsigma" . "Σ")
         ("mu" . "μ")
      ))
 (setq prettify-symbols-unprettify-at-point 'right-edge)
 (add-hook 'org-mode-hook 'prettify-symbols-mode)

(setq
    locale-coding-system 'utf-8
    set-terminal-coding-system 'utf-8
    set-keyboard-coding-system 'utf-8
    set-selection-coding-system 'utf-8
    prefer-coding-system 'utf-8)

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
     (org-agenda nil "i"))
(defun zk/switch-to-week-review ()
     (interactive)
     (org-agenda nil "w"))
    ;;(global-set-key (kbd "C-S c") 'avy-goto-char)

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
 (global-set-key (kbd "C-x <up>") 'windmove-up)
 (global-set-key (kbd "C-x <down>") 'windmove-down)
 (global-set-key (kbd "C-x <right>") 'windmove-right)
 (global-set-key (kbd "C-x <left>") 'windmove-left)

 (global-unset-key (kbd "C-x k"))
 (global-unset-key (kbd "C-x j"))
 (global-unset-key (kbd "C-x l"))
 (global-unset-key (kbd "C-x h"))

 (global-set-key (kbd "C-x k") 'windmove-up)
 (global-set-key (kbd "C-x j") 'windmove-down)
 (global-set-key (kbd "C-x l") 'windmove-right)
 (global-set-key (kbd "C-x h") 'windmove-left)

 ;; Resize windows
 (global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
 (global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
 (global-set-key (kbd "C-M-<down>") 'shrink-window)
 (global-set-key (kbd "C-M-<up>") 'enlarge-window)

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

(use-package rainbow-delimiters
 :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'shell-mode-hook 'rainbow-delimiters-mode))

(setq evil-want-keybinding nil)                   ;; this statement is required to enable evil/evil-colleciton mode
(evil-mode 1)                                     ;; enable evil-mode
(setq evil-want-abbrev-expand-on-insert-exit nil)
(use-package evil-collection                      ;; evil-friendly binding for many modes
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-magit
  :after evil)
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
         "g" 'magit-status
         "r" 'bookmark-bmenu-list
         "l" 'org-store-link
         "L" 'org-insert-link
         "n" 'org-noter
         "q" 'kill-current-buffer)
        (evil-leader-mode 1)
        (global-evil-leader-mode 1))
         ;;"B" 'zetteldeft-backlink-add
         ;;"s" 'zk/gen-scratch-buffer

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

(use-package zetteldeft
  :ensure t
  :after deft
  :config (zetteldeft-set-classic-keybindings))

(use-package avy
  :ensure t)
  ;; ("C-S-l" . avy-goto-line)

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

;;turn on everywhere
(global-undo-tree-mode 1)
;; Save history to a file
(setq
    undo-tree-auto-save-history 1 ; Show relative times in the undo tree visualizer
    undo-tree-visualizer-timestamps 1; Show diffs when browsing through the undo tree
    undo-tree-visualizer-diff 1)

(use-package magit
  :init
   (setq magit-push-current-set-remote-if-missing nil)
  :config
   (setq magit-push-always-verify nil))

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
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    ;;(global-set-key (kbd "C-x l") 'counsel-locate)
    ;;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(use-package projectile
:ensure t
:config
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1))
(setq projectile-project-search-path '("~/.config/" "~/dox/wrk/"))

(use-package jedi
 :ensure t
 :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
 )
(setq jedi:complete-on-dot t)

;; (add-to-list 'company-backends 'company-jedi)

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

(use-package flycheck
:ensure t
:init (global-flycheck-mode))

(use-package elpy
  :init
  :disabled t
  (elpy-enable)
)

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

(setq zk/org-agenda-directory "~/org/gtd/"
    org-agenda-files '("~/org/gtd" );; org-agenda-files
    org-agenda-clockreport-parameter-plist        ;; Clock report config
    '(:link t :maxlevel 7 :fileskip0 t  :compact t :narrow 80 :formula %)
    org-agenda-start-on-weekday 0                   ;; Weekday start on Sunday
    org-agenda-dim-blocked-tasks nil                ;; Do not dim blocked tasks
    org-agenda-inhibit-startup t                    ;; Stop preparing agenda buffers on startup:
    org-agenda-use-tag-inheritance nil              ;; Disable tag inheritance for agendas:
    org-agenda-tags-column -140                     ;; Set tags far to the right
    org-clock-out-remove-zero-time-clocks t         ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
    org-clock-persist t                             ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    org-use-fast-todo-selection t                   ;; from any todo state to any other state; using it keys
    org-treat-S-cursor-todo-selection-as-state-change nil;; S-R,S-L skip the note/log info[used when fixing the state]
    org-agenda-window-setup 'only-window            ;; Always open my agenda in fullscreen
    org-agenda-deadline-leaders '("!D!: " "D%2d: " "")
    org-agenda-scheduled-leaders '("" "S%3d: ")
    org-columns-default-format "%50ITEM %TODO %3PRIORITY %10TAGS %17Effort(Estimated Effort){:} %12CLOCKSUM(Time Spent)"
    org-agenda-prefix-format
      '((agenda . "%-12c%?-12t% s")
        (timeline . "% s")
        (todo . "%-12c")
        (tags . "%-12c")
        (search . "%-12c")))

(org-clock-persistence-insinuate)                   ;; Resume clocking task when emacs is restarted
(add-hook 'org-agenda-mode-hook                     ;; disable line-number when i open org-agenda view
          (lambda() (display-line-numbers-mode -1)))

;; agenda custom view
;; Compact the block agenda view (disabled)
(setq org-agenda-compact-blocks nil)

;; Set the times to display in the time grid
;; (setq org-agenda-time-grid
;;       (quote
;;        ((daily today remove-match)
;;         (800 1200 1600 2000)
;;         "......" "----------------")))
(setq org-agenda-grid nil)
(setq org-agenda-block-separator 9472)     ; use 'straight line' as a block-agenda divider
(custom-set-variables
 '(org-agenda-custom-commands
   '(
     ("i" "MyAgenda"
      ((agenda ""
               ((org-agenda-overriding-header "Today's Schedule")
                (org-agenda-span 'day)
                (org-deadline-warning-days 365)
                (org-agenda-time-grid nil)
                ;; Ignore books
                (org-agenda-skip-function '(org-agenda-skip-if 'regexp "books"))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("READ" "READING")))
                 ))
       ;; (todo "NEXT"
       ;;       ((org-agenda-overriding-header "In Progress")
       ;;        (org-agenda-files
       ;;         (quote
       ;;          ("~/org/gtd/someday.org" "~/org/gtd/projects.org" "~/org/gtd/next.org" "~/org/gtd/habits.org")))))
       (todo "READING"
             ((org-agenda-overriding-header "My Books")
              (org-agenda-files
                '("~/org/books.org"))))
       (todo "TODO"
             ((org-agenda-overriding-header "To Refile")
              (org-agenda-files
                '("~/org/gtd/inbox.org"))))
       (todo "TODO"
             ((org-agenda-overriding-header "Projects")
              (org-agenda-files
                '("~/org/gtd/projects.org"))))
       (todo "TODO"
             ((org-agenda-overriding-header "Emails")
              (org-agenda-compact-blocks t)
              (org-agenda-files
                '("~/org/gtd/emails.org"))))
       (todo "TODO"
             ((org-agenda-overriding-header "One-off Tasks")
              (org-agenda-files '("~/org/gtd/next.org"))
              (org-agenda-compact-blocks t)
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
       nil))
           ("w" "My Weekly Review"
            ((agenda ""
             ((org-agenda-overriding-header "Week in Review")
              (org-agenda-span 'week)
              (org-agenda-start-day "-7d")
              (org-agenda-start-on-weekday 0)
              (org-agenda-files
                '("~/org/archive.org"))
              (org-agenda-archives-mode t)
              (org-agenda-time-grid nil)
              (org-agenda-prefix-format"%i %-12:c %?-t\t ")
             ))))
           )))

;; PS: check out the original code from here:
;; https://github.com/gjstein/emacs.d/blob/master/config/gs-org.el
;; there is function that check if the task is a project or not
;; i just removed it here

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

(defvar zk/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun zk/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " zk/org-current-effort) nil nil zk/org-current-effort)))
(setq zk/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil zk/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun zk/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (interactive)
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'zk/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

;;(defvar zk/org-agenda-bulk-process-key ?f
;;  "Default key for bulk processing inbox items.")
;;
;;(defun zk/org-process-inbox ()
;;  "Called in org-agenda-mode, processes all inbox items."
;;  (interactive)
;;  (org-agenda-bulk-mark-regexp "inbox:")
;;  (zk/bulk-process-entries))



;;(defun jethro/org-agenda-process-inbox-item ()
;;  "Process a single item in the org-agenda."
;;  (org-with-wide-buffer
;;   (org-agenda-set-tags)
;;   (org-agenda-priority)
;;   (call-interactively 'jethro/my-org-agenda-set-effort)
;;   (org-agenda-refile nil nil t)))
;;
;;(defun jethro/bulk-process-entries ()
;;  (if (not (null org-agenda-bulk-marked-entries))
;;      (let ((entries (reverse org-agenda-bulk-marked-entries))
;;            (processed 0)
;;            (skipped 0))
;;        (dolist (e entries)
;;          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
;;            (if (not pos)
;;                (progn (message "Skipping removed entry at %s" e)
;;                       (cl-incf skipped))
;;              (goto-char pos)
;;              (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
;;              ;; `post-command-hook' is not run yet.  We make sure any
;;              ;; pending log note is processed.
;;              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
;;                        (memq 'org-add-log-note post-command-hook))
;;                (org-add-log-note))
;;              (cl-incf processed))))
;;        (org-agenda-redo)
;;        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
;;        (message "Acted on %d entries%s%s"
;;                 processed
;;                 (if (= skipped 0)
;;                     ""
;;                   (format ", skipped %d (disappeared before their turn)"
;;                           skipped))
;;                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))
;;
;;(defun jethro/org-inbox-capture ()
;;  (interactive)
;;  "Capture a task in agenda mode."
;;  (org-capture nil "i"))
;;
;;(setq org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key jethro/org-agenda-process-inbox-item)))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 48)
;;(setq org-habit-show-habits-only-for-today t)

;; The default todo-keywords
(setq org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n/!)" "|" "DONE(d/!)")
         (sequence "WAITING(w@/!)" "HOLD(h/!)" "|" "FAILED(f@/!)" "CANCELLED(c@/!)"))))
 ;; General todo-keyword faces
(setq org-todo-keyword-faces
 '(
   ("TODO" . (:foreground "brown2" :weight bold))
   ("NOTE" . (:foreground "brown2" :weight bold))
   ("READ" . (:foreground "brown2" :weight bold))

   ("NEXT" . (:foreground "#00b0d1"  :weight bold ))
   ("READING" . (:foreground "#00b0d1"  :weight bold ))

   ("DONE" . (:foreground "#16a637" :weight bold))

   ("HOLD" . (:foreground "orange"  :weight bold))
   ("WAITING" . (:foreground "orange"  :weight bold))

   ("FAILED" . (:foreground "orange red" :weight bold))

   ("CANCELED" . (:foreground "gray" :background "red1" :weight bold))
 ))
 ;;  hl-todo-keyword-faces [give it a try]
 ;; Auto-update tags whenever the state is changed
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        (done ("WAITING") )
        ("TODO" ("WAITING") ("CANCELLED"))
        ("NEXT" ("WAITING") ("CANCELLED"))
        ("FAILED" ("FAILED"))
        ("DONE" ("WAITING") ("CANCELLED"))))

;; Add all file in '~/org/gtd'
(require 'org-protocol)
(require 'org-capture)
(setq org-capture-templates
      `(
     ("i" "inbox" entry
      (file ,(concat zk/org-agenda-directory "inbox.org"))
      "* TODO %?" )

     ("j" "Journal" entry
      (file+olp+datetree "~/org/journal.org.gpg")
           "* %?
             Added: %U")

     ("e" "email" entry
      (file+headline ,(concat zk/org-agenda-directory "emails.org") "Emails")
      "* TODO [#A] Reply: %a :@home:@school:" :immediate-finish t)

     ("l" "link" entry
      (file ,(concat zk/org-agenda-directory "inbox.org"))
      "* TODO [#C] %(org-cliplink-capture)" :immediate-finish t)

     ("p" "org-protocol-capture" entry
      (file ,(concat zk/org-agenda-directory "inbox.org"))
      "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)

     ;; a template for my code snippets
     ("c" "Code Snippets"  entry
      (file ,(concat zk/org-agenda-directory "inbox.org"))
      "* TODO %A :code:\n#+begin_src %^{language}\n %i \n#+end_src"
      :immediate-finish t)

     ;; a template for my ideas : ** IDEA/DONE [timestamp]: the idea
     ("d" "Ideas"  entry
      (file "~/org/ideas.org")
      "* TODO %U: %?  %i\n ")

     ;; a template for my feeling good stuff : -[timestamp]: /string/
     ("f" "Things make me feel good" item
      (file "~/org/feelGood.org")
      "- %U: %?  %i\n\n ")

     ("t" "Thoughts" item
      (file "~/org/thoughts.org")
      "- %U: %?  %i\n\n ")))

;; Refiling [need reading]
(setq org-refile-use-outline-path 'file
 org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("next.org" :level . 0)
                           ("someday.org" :level . 0)
                           ("~/org/links.org" :level . 1)
                           ("~/org/ideas.org" :level . 1)
                           ("projects.org" :maxlevel . 2)))

(add-to-list 'org-latex-classes
                 '("elsarticle"
                   "\\documentclass{elsarticle}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
                 '("mimosis"
                   "\\documentclass{mimosis}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]
\\newcommand{\\mboxparagraph}[1]{\\paragraph{#1}\\mbox{}\\\\}
\\newcommand{\\mboxsubparagraph}[1]{\\subparagraph{#1}\\mbox{}\\\\}"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\mboxparagraph{%s}" . "\\mboxparagraph*{%s}")
                   ("\\mboxsubparagraph{%s}" . "\\mboxsubparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '( "koma-article"
                "\\documentclass{scrartcl}"
                ( "\\section{%s}" . "\\section*{%s}" )
                ( "\\subsection{%s}" . "\\subsection*{%s}" )
                ( "\\subsubsection{%s}" . "\\subsubsection*{%s}" )
                ( "\\paragraph{%s}" . "\\paragraph*{%s}" )
                ( "\\subparagraph{%s}" . "\\subparagraph*{%s}" )))
;; Coloured LaTeX using Minted
(setq org-latex-listings 'minted
    org-latex-packages-alist '(("" "minted"))
    org-latex-pdf-process
    '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "biber %b"
      "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;; syntex-highlighting
(use-package htmlize)
;;Don’t include a footer...etc in exported HTML document.
(setq org-html-postamble nil)
(setq org-src-window-setup 'current-window)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)

(setq reftex-default-bibliography '("~/org/files/bib/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/org/files/bib/notes.org"
      org-ref-default-bibliography '("~/org/files/bib/references.bib")
      org-ref-pdf-directory "~/dox/papers/")

(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(use-package pdf-tools
:ensure t
:config
(pdf-tools-install))
 ;; open pdfs scaled to fit page
(setq-default pdf-view-display-size 'fit-page)
 ;; exchange isearch -- occur, occur -- isearch
(define-key pdf-view-mode-map (kbd "C-s") 'occur)
(define-key pdf-view-mode-map (kbd "M-s o") 'isearch-forward)
;; turn off cua so copy works
(add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
;; more fine-grained zooming
(setq pdf-view-resize-factor 1.1)

(use-package org-pdfview
:ensure t)
;; Set the pdf-view incompatible-modes[linum mode: line numbers]
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))

;; org noter configuration
(use-package org-noter
 :after org
 :ensure t
 :config (setq org-noter-auto-save-last-location t
               org-noter-doc-split-fraction (quote (0.7 . 0.7))
               org-noter-notes-window-behavior nil
               org-noter-always-create-frame nil
               org-noter-separate-notes-from-heading t))

(use-package ox-reveal
  :ensure ox-reveal
  )
(setq org-reveal-root "file:///home/zakaria/org/files/conf/revealJS/reveal.js-4.1.0")
(setq org-reveal-mathjax t)

(eval-after-load "org"
  (use-package ob-async
    :ensure t
    :init (require 'ob-async)))
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-tab-acts-natively t)
        (org-babel-do-load-languages
        'org-babel-load-languages
        '((python . t)
          (shell . t)
          (emacs-lisp . t)
          (R . t)
          ))
