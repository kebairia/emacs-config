;; -*- lexical-binding: t -*-
;; This file has been generated from init.org file. DO NOT EDIT.
;; Sources are available from https://github.com/kebairia/emacs-config

;; Copyright (C) 2023 Kebairia Zakaria

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

(defvar my/init-start-time (current-time) "Time when init.el was started")
(defvar my/section-start-time (current-time) "Time when section was started")
(defun my/report-time (section)
  (message "%-36s %.2fs"
           (concat section " " "section time: ")
           (float-time (time-subtract (current-time) my/section-start-time))))
(message "---------------------------------------------------------------")

(setq package-enable-at-startup nil)
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path
             (expand-file-name "theme" user-emacs-directory))

(setq package-list
      '(cape                ; Completion At Point Extensions
        orderless           ; Completion style for matching regexps in any order
        vertico             ; VERTical Interactive COmpletion
        marginalia          ; Enrich existing commands with completion annotations
        aggressive-indent   ; Keeps your code always indented 
        evil                ; A VI layer for Emacs
        evil-collection
        evil-org            ; Evil extensions for Org-mode
        org-roam            ; Roam implementation for Emacs/org
        org-roam-ui         ; A graph generator for org-roam notes
        org-appear          ; Make invisible parts of Org elements appear visible.
        org-cliplink        ; Insert org-mode links from clipboard
        org-pdftools        ; A custom org link type for pdf-tools
        org-noter-pdftools  ; Support for org-noter 
        org-noter           ; Emacs document annotator, using Org-mode
        f                   ; Modern API for working with files and directories
        corfu               ; Completion Overlay Region FUnction
        deft                ; Quickly browse, filter, and edit plain text notes
        citar               ; Citation-related commands for org, latex, markdown
        citeproc            ; A CSL 1.0.2 Citation Processor
        flyspell-correct-popup ; Correcting words with flyspell via popup interface
        flyspell-popup      ; Correcting words with Flyspell in popup menus
        helpful             ; A better help buffer
        htmlize             ; Convert buffer text and decorations to HTML
        mini-frame          ; Show minibuffer in child frame on read-from-minibuffer
        imenu-list          ; Show imenu entries in a separate buffer
        eglot               ; A client for Language Server Protocol servers
        magit               ; A Git porcelain inside Emacs.
        markdown-mode       ; Major mode for Markdown-formatted text
        use-package         ; A configuration macro for simplifying your .emacs
        vc-backup           ; VC backend for versioned backups
        yaml-mode           ; YAML mode
        hcl-mode            ; HCL mode
        jinja2-mode         ; Jinja2 mode
        ;; org-auto-tangle     ; Tangle org file when it is saved
        which-key))         ; Display available keybindings in popup

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

;; Special case for pdf-tools that has recently (2022) changed maintainer
(straight-use-package
 '(pdf-tools :type git :host github :repo "vedang/pdf-tools"))

;; ;; Denote not yet on ELPA (2022-06-19)
;; (straight-use-package
;;  '(denote :type git :host github :repo "protesilaos/denote"))

;; Display org properties in the agenda buffer (modified version)
;; (straight-use-package
;;  '(org-agenda-property :type git :host github :repo "Malabarba/org-agenda-property"
;;                        :fork (:host github :repo "rougier/org-agenda-property")))

;; NANO splash
;; (straight-use-package
;;  '(nano-splash :type git :host github :repo "rougier/nano-splash"))

;; NANO theme
(straight-use-package
 '(nano-theme :type git :host github :repo "rougier/nano-theme"))

;; NANO modeline
(straight-use-package
 '(nano-modeline :type git :host github :repo "rougier/nano-modeline"))

;; NANO agenda
(straight-use-package
 '(nano-agenda :type git :host github :repo "rougier/nano-agenda"))

;; NANO agenda
(straight-use-package
 '(minibuffer-header :type git :host github :repo "rougier/minibuffer-header"))

;; SVG tags, progress bars & icons
(straight-use-package
 '(svg-lib :type git :host github :repo "rougier/svg-lib"))

;; Replace keywords with SVG tags
(straight-use-package
 '(svg-tag-mode :type git :host github :repo "rougier/svg-tag-mode"))

;; ;; Relative date formatting
;; (straight-use-package
;;  '(relative-date :type git :host github :repo "rougier/relative-date"))

;; org imenu
;; (straight-use-package
;;  '(org-imenu :type git :host github :repo "rougier/org-imenu"))

;; Bilbliography manager in org mode
(straight-use-package
  '(org-bib :type git :host github :branch "org-imenu" :repo "rougier/org-bib-mode"))

(setq-default
 inhibit-startup-screen t               ; Disable start-up screen
 inhibit-startup-message t              ; Disable startup message
 inhibit-startup-echo-area-message t    ; Disable initial echo message
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 initial-buffer-choice t)               ; Open *scratch* buffer at init

(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
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
      vc-make-backup-files t       ; No backup of files under version contr
      backup-by-copying t          ; Don't clobber symlinks
      version-control t            ; Version numbers for backup files
      delete-old-versions t        ; Delete excess backup files silently
      kept-old-versions 6          ; Number of old versions to keep
      kept-new-versions 9          ; Number of new versions to keep
      delete-by-moving-to-trash t) ; Delete files to trash

;; Back
(require 'vc-backup)

(setq bookmark-default-file (expand-file-name "bookmark" user-emacs-directory))

(require 'recentf)

(setq recentf-max-menu-items 10
      recentf-max-saved-items 100)

(let (message-log-max)
  (recentf-mode 1))

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

(setq save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-forget-unreadable-files t)

(save-place-mode 1)

(setq custom-file (concat user-emacs-directory "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file nil t))

(require 'server)

(unless (server-running-p)
  (server-start))

(defun zk/split-go-right ()
  "Split the current window horizontally and move to the new window on the right."
  (interactive)
  (split-window-horizontally)
  (windmove-right))

(defun zk/split-go-down ()
  "Split the current window vertically and move to the new window below."
  (interactive)
  (split-window-vertically)
  (windmove-down))

(bind-key "C-c C" (lambda () (interactive) (find-file "~/.config/emacs/init.org")))
(bind-key "C-c b" (lambda () (interactive) (find-file "~/org/books.org")))

(bind-key "M-n" 'switch-to-next-buffer)
(bind-key "M-p" 'switch-to-prev-buffer)

(bind-key "C-c k" #'windmove-up)
(bind-key "C-c j" #'windmove-down)
(bind-key "C-c l" #'windmove-right)
(bind-key "C-c h" #'windmove-left)

(bind-key "C-c i" #'zk/split-go-right)
(bind-key "C-c m" #'zk/split-go-down)

(bind-key "C-c c" #'org-capture)

(bind-key "C-c t" #'imenu-list)
(bind-key "C-c T" #'imenu)

(my/report-time "Core")

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
                            (menu-bar-lines . 1)))

;; Default frame settings
(setq initial-frame-alist default-frame-alist)

;; (bind-key "M-n"        #'my/make-frame)
(bind-key "C-x C-c"    #'my/kill-emacs)
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

;; Make a window dedicated
(defun my/toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
     (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer))
  (force-window-update))

(bind-key "C-c d" #'my/toggle-window-dedicated)

(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(bind-key "C-x k" #'kill-current-buffer)

;; Use Ibuffer for Buffer List
(bind-key "C-x C-b" #'ibuffer)
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

(tooltip-mode -1)                       ; No tooltips
(scroll-bar-mode -1)                    ; No scroll bars
(tool-bar-mode -1)                      ; No toolbar

(menu-bar-mode nil)

(require 'which-key)

(which-key-mode)

(setq-default cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
              cursor-type '(hbar . 2)            ; Underline-shaped cursor
              cursor-intangible-mode t           ; Enforce cursor intangibility
              x-stretch-cursor nil)              ; Don't stretch cursor to the glyph width

(blink-cursor-mode 0)                            ; Still cursor

(setq-default use-short-answers t                     ; Replace yes/no prompts with y/n
              confirm-nonexistent-file-or-buffer nil) ; Ok to visit non existent files

(delete-selection-mode 1)

(defun my/fill-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  
  (interactive)
  (let ((fill-column
         (if (eq last-command #'my/fill-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(bind-key "M-q"  #'my/fill-unfill)
;; (bind-key [remap fill-paragraph]  #'my/fill-unfill)

(setq-default visible-bell nil             ; No visual bell      
              ring-bell-function 'ignore)  ; No bell

(setq-default mouse-yank-at-point t) ; Yank at point rather than pointer
(mouse-avoidance-mode 'exile)        ; Avoid collision of mouse with point

(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") #'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") #'scroll-up-line))

(setq-default scroll-conservatively 101       ; Avoid recentering when scrolling far
              scroll-margin 2                 ; Add a margin when scrolling vertically
              recenter-positions '(5 bottom)) ; Set re-centering positions

(setq-default select-enable-clipboard t) ; Merge system's and Emacs' clipboard

(defun my/paste-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun my/copy-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (and (not (display-graphic-p))
           (eq system-type 'darwin))
  (setq interprogram-cut-function   #'my/copy-to-osx
        interprogram-paste-function #'my/paste-from-osx))

(setq help-window-select t)             ; Focus new help windows when opened

(bind-key "C-h f"   #'helpful-callable) ; Look up callable
(bind-key "C-h v"   #'helpful-variable) ; Look up variable
(bind-key "C-h k"   #'helpful-key)      ; Look up key 
(bind-key "C-c C-d" #'helpful-at-point) ; Look up the current symbol at point
(bind-key "C-h F"   #'helpful-function) ; Look up *F*unctions (excludes macros).
(bind-key "C-h C"   #'helpful-command)  ; Look up *C*ommands.

(require 'nano-theme)
;; (setq nano-fonts-use t) ; Use theme font stack
(nano-modeline-mode)    ; Use nano-modeline

(my/report-time "Interface")

(setq my/section-start-time (current-time))

(require 'nano-theme)
(setq nano-fonts-use t) ; Use theme font stack
(nano-dark)             ; Use theme dark version
(nano-mode)             ; Recommended settings


(defun my/set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))
(my/set-face 'italic 'nano-faded)

(defun load-nano-dark-theme-after-startup ()
  "A custom function to be executed after Emacs startup."
  (nano-dark))

(add-hook 'after-init-hook 'load-nano-dark-theme-after-startup)

;; (require 'nano-splash)

(set-face-attribute 'default nil
                    :family "FantasqueSansMono"
                    :weight 'light
                    :height 140)

(set-face-attribute 'bold nil
                    :family "FantasqueSansMono"
                    :weight 'regular)

(set-face-attribute 'italic nil
                    :family "FantasqueSansMono"
                    :weight 'semilight
                    :slant 'italic)

(set-fontset-font t 'unicode
                    (font-spec :name "FantasqueSansMono"
                               :size 16) nil)

(set-fontset-font t '(#xe000 . #xffdd)
                     (font-spec :name "FantasqueSansMono"
                                :size 12) nil)

(setq-default fill-column 80                          ; Default line width 
              sentence-end-double-space nil           ; Use a single space after dots
              bidi-paragraph-direction 'left-to-right ; Faster
              truncate-string-ellipsis "…")           ; Nicer ellipsis

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

(when (eq system-type 'darwin)
  (add-hook 'term-mode-hook
            (lambda ()
              (setq buffer-display-table (make-display-table)))))

(setq x-underline-at-descent-line nil
      x-use-underline-position-properties t
      underline-minimum-offset 10)

(my/report-time "Visual")

(setq my/section-start-time (current-time))

(setq evil-want-C-i-jump nil)
(setq evil-want-abbrev-expand-on-insert-exit nil)
(setq evil-want-keybinding nil)

(evil-mode 1)
(when (require 'evil-collection nil t)
(evil-collection-init))

;; (clear-abbrev-table 'global-abbrev-table)
(define-abbrev-table 'global-abbrev-table
  '(
    ;; Words and Sentences
    ("afaik" "as far as I know")
    ("emacs" "Emacs")
    ("realy" "really")
    ("mnm" "Millennium")
    ("thru" "through")
    ("i" "I")
    ("envr" "environment")
    ("thanx" "thanks")
    ("btw" "by the way")
    ;; Emojies 
    ("hrt" "❤")
    (":)" "😀")
    ;; Arrows 
    ("ra" "→")
    ("la" "←")
    ("lra" "⟶")
    ("lla" "⟵")
    ))

(abbrev-mode)

(global-aggressive-indent-mode 1)

(setq-default initial-major-mode 'text-mode   ; Initial mode is text
              default-major-mode 'text-mode)  ; Default mode is text

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)

(setq-default indent-tabs-mode nil        ; Stop using tabs to indent
              tab-always-indent 'complete ; Indent first then try completions
              tab-width 2)                ; Smaller width for tab characters

;; Let Emacs guess Python indent silently
(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

(require 'paren)
;; (setq show-paren-style 'expression)
(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-when-point-inside-paren nil)
(show-paren-mode)

(electric-pair-mode)

(require 'imenu-list)

(setq-default imenu-list-position 'left
              imenu-max-item-length 1000)

(require 'hl-line)

(global-hl-line-mode)

;; (require 'pdf-tools)

(add-hook 'doc-view-mode-hook 'pdf-tools-install)

(setq-default pdf-view-use-scaling t
              pdf-view-use-imagemagick nil)

(my/report-time "Editing")

(setq my/section-start-time (current-time))

(require 'corfu)

(setq corfu-cycle t                ; Enable cycling for `corfu-next/previous'
      corfu-auto t                 ; Enable auto completion
      corfu-auto-delay 0        ; Delay before auto-completion shows up
      corfu-auto-prefix 2
      completion-styles '(basic)
      corfu-separator ?\s          ; Orderless field separator
      corfu-quit-at-boundary nil   ; Never quit at completion boundary
      corfu-quit-no-match t        ; Quit when no match
      corfu-preview-current t    ; Disable current candidate preview
      corfu-preselect-first nil    ; Disable candidate preselection
      corfu-on-exact-match nil     ; Configure handling of exact matches
      corfu-echo-documentation 0.25 ; Disable documentation in the echo area
      corfu-scroll-margin 5)       ; Use scroll margin

(add-hook 'prog-mode-hook 'corfu-mode)

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

(require 'orderless)
  
(setq completion-styles '(substring orderless basic)
      orderless-component-separator 'orderless-escapable-split-on-space
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(my/report-time "Completion")

(setq my/section-start-time (current-time))

(require 'vertico)

;; (setq completion-styles '(basic substring partial-completion flex))

(setq vertico-resize nil        ; How to resize the Vertico minibuffer window.
      vertico-count 8           ; Maximal number of candidates to show.
      vertico-count-format nil) ; No prefix with number of entries

(vertico-mode)

(setq vertico-grid-separator
      #("  |  " 2 3 (display (space :width (1))
                             face (:background "#ECEFF1")))

      vertico-group-format
      (concat #(" " 0 1 (face vertico-group-title))
              #(" " 0 1 (face vertico-group-separator))
              #(" %s " 0 4 (face vertico-group-title))
              #(" " 0 1 (face vertico-group-separator
                          display (space :align-to (- right (-1 . right-margin) (- +1)))))))

(set-face-attribute 'vertico-group-separator nil
                    :strike-through t)
(set-face-attribute 'vertico-current nil
                    :inherit '(nano-strong nano-subtle))
(set-face-attribute 'completions-first-difference nil
                    :inherit '(nano-default))

(bind-key "<backtab>" #'minibuffer-complete vertico-map)

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(defun minibuffer-format-candidate (orig cand prefix suffix index _start)
  (let ((prefix (if (= vertico--index index)
                    "  "
                  "   "))) 
    (funcall orig cand prefix suffix index _start)))

(advice-add #'vertico--format-candidate
           :around #'minibuffer-format-candidate)

(defun vertico--prompt-selection ()
  "Highlight the prompt"

  (let ((inhibit-modification-hooks t))
    (set-text-properties (minibuffer-prompt-end) (point-max)
                         '(face (nano-strong nano-salient)))))

(defun minibuffer-vertico-setup ()

  (setq truncate-lines t)
  (setq completion-in-region-function
        (if vertico-mode
            #'consult-completion-in-region
          #'completion--in-region)))

(add-hook 'vertico-mode-hook #'minibuffer-vertico-setup)
(add-hook 'minibuffer-setup-hook #'minibuffer-vertico-setup)

(require 'marginalia)

(setq-default marginalia--ellipsis "…"    ; Nicer ellipsis
              marginalia-align 'right     ; right alignment
              marginalia-align-offset -1) ; one space on the right

(marginalia-mode)

(require 'nano-theme)
(require 'nano-modeline)

(setq nano-modeline-prefix 'status)
(setq nano-modeline-prefix-padding 1)

(set-face-attribute 'header-line nil)
(set-face-attribute 'mode-line nil
                    :foreground (face-foreground 'nano-subtle-i)
                    :background (face-foreground 'nano-subtle-i)
                    :inherit nil
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground (face-foreground 'nano-subtle-i)
                    :background (face-foreground 'nano-subtle-i)
                    :inherit nil
                    :box nil)

(set-face-attribute 'nano-modeline-active nil
                    :underline (face-foreground 'nano-default-i)
                    :background (face-background 'nano-subtle)
                    :inherit '(nano-default-)
                    :box nil)
(set-face-attribute 'nano-modeline-inactive nil
                    :foreground 'unspecified
                    :underline (face-foreground 'nano-default-i)
                    :background (face-background 'nano-subtle)
                    :box nil)

(set-face-attribute 'nano-modeline-active-name nil
                    :foreground "white"
                    :inherit '(nano-modeline-active nano-strong))
(set-face-attribute 'nano-modeline-active-primary nil
                    :inherit '(nano-modeline-active))
(set-face-attribute 'nano-modeline-active-secondary nil
                    :inherit '(nano-faded nano-modeline-active))

;; (set-face-attribute 'nano-modeline-active-status-RW nil
;;                     :inherit '(nano-faded-i nano-strong nano-modeline-active))
;; (set-face-attribute 'nano-modeline-active-status-** nil
;;                     :inherit '(nano-popout-i nano-strong nano-modeline-active))
;; (set-face-attribute 'nano-modeline-active-status-RO nil
;;                     :inherit '(nano-default-i nano-strong nano-modeline-active))

(set-face-attribute 'nano-modeline-inactive-name nil
                    :inherit '(nano-faded nano-strong
                               nano-modeline-inactive))
(set-face-attribute 'nano-modeline-inactive-primary nil
                    :inherit '(nano-faded nano-modeline-inactive))

(set-face-attribute 'nano-modeline-inactive-secondary nil
                    :inherit '(nano-faded nano-modeline-inactive))
(set-face-attribute 'nano-modeline-inactive-status-RW nil
                    :inherit '(nano-modeline-inactive-secondary))
(set-face-attribute 'nano-modeline-inactive-status-** nil
                    :inherit '(nano-modeline-inactive-secondary))
(set-face-attribute 'nano-modeline-inactive-status-RO nil
                    :inherit '(nano-modeline-inactive-secondary))

(defun my/thin-modeline ()
  "Transform the modeline in a thin faded line"
  
  (nano-modeline-face-clear 'mode-line)
  (nano-modeline-face-clear 'mode-line-inactive)
  (setq mode-line-format (list ""))
  (setq-default mode-line-format (list ""))
  (set-face-attribute 'mode-line nil
                      :box nil
                      :inherit nil
                      :foreground (face-background 'nano-subtle)
                      :background (face-background 'nano-subtle)
                      :height 0.1)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :inherit nil
                      :foreground (face-background 'nano-subtle)
                      :background (face-background 'nano-subtle)
                      :height 0.1))

(add-hook 'nano-modeline-mode-hook #'my/thin-modeline)

(nano-modeline-mode 1)

(my/report-time "Minibuffer/Modeline")

(setq org-roam-directory (file-truename "/home/zakaria/dox/braindump/org-files"))
(org-roam-db-autosync-mode)                    ; autosync for db
(setq org-roam-dailies-directory (file-truename "/home/zakaria/org/daily")) ; directory for my dailies
(setq org-roam-db-gc-threshold most-positive-fixnum) ; Garbage collection

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

(setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

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

(require 'org-noter)
(bind-key "C-c n n" 'org-noter-insert-note)
(bind-key "C-c n N" 'org-noter-insert-precise-note)

(setq org-noter-auto-save-last-location t
      org-noter-doc-split-fraction (quote (0.7 . 0.7))
      org-noter-notes-window-behavior nil
      org-noter-notes-window-location "Vertical"
      org-noter-always-create-frame nil
      org-noter-separate-notes-from-heading t)

(setq my/section-start-time (current-time))

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
        ("TODO" . (:foreground "brown2" :weight bold)) ; brown foreground color, bold
        ("READ" . (:foreground "brown2" :weight bold)) ; brown foreground color, bold
        
        ("NEXT" . (:foreground "#00b0d1"  :weight bold )) ; blue-green foreground color, bold
        ("READING" . (:foreground "#00b0d1"  :weight bold )) ; blue-green foreground color, bold
        
        ("DONE" . (:foreground "#16a637" :weight bold)) ; green foreground color, bold
        
        ("HOLD" . (:foreground "orange"  :weight bold)) ; orange foreground color, bold
        
        ("CANCELED" . (:foreground "gray" :background "red1" :weight bold)) ; gray foreground color, red background color, bold
        ))

(setq org-capture-templates
      `(
        ;; Inbox entry
        ("i" " inbox" entry (file "~/org/gtd/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))

        ;; Post entry
        ("p" " post" entry (file "~/org/posts.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))

        ;; Link entry
        ("L" " link" entry (file+headline "~/org/gtd/inbox.org" "Links")
         ,(concat "* TODO %a %?\n"
                  "/Entered on/ %U") :immediate-finish t)

        ;; Slipbox entry
        ("s" " slipbox" entry (file "~/dox/braindump/org-files/fleetnotes.org")
         "* %<%a, %d %b %y (%H:%M)> : %?\n")
        ))

(require 'org-protocol)

(setq org-appear-autolinks t
      org-appear-autosubmarkers t)
(add-hook 'org-mode-hook 'org-appear-mode)

(require 'org-cliplink)

(setq-default org-src-fontify-natively t         ; Fontify code in code blocks
              org-adapt-indentation nil          ; Adaptive indentation
              org-src-tab-acts-natively t        ; Tab acts as in source editing
              org-confirm-babel-evaluate nil     ; No confirmation before executing code
              org-edit-src-content-indentation 0 ; No relative indentation for code blocks
              org-fontify-whole-block-delimiter-line t) ; Fontify whole block

;; Add languages to babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)
   (R . t)
   ))

(my/report-time "Org")

(defun zk/switch-to-agenda ()
  (interactive)
  (org-agenda nil "g"))

(bind-key "C-c a" #'zk/switch-to-agenda)
(bind-key "C-c w" #'org-agenda-week-view)

(require 'evil-org)
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(setq org-agenda-directory "~/org/gtd/"
      org-agenda-files '("~/org/gtd")) ;; org-agenda-files

(setq org-agenda-dim-blocked-tasks nil ;; Do not dim blocked tasks
      org-agenda-span 'day ;; Show one day
      org-agenda-inhibit-startup t ;; Stop preparing agenda buffers on startup
      org-agenda-use-tag-inheritance nil ;; Disable tag inheritance for agendas
      org-agenda-show-log t
      org-agenda-skip-scheduled-if-deadline-is-shown t ;; Skip scheduled if already shown as a deadline
      org-agenda-deadline-leaders '("!D!: " "D%2d: " "")
      org-agenda-scheduled-leaders '("" "S%3d: ")
      org-agenda-start-on-weekday 0 ;; Weekday start on Sunday
      org-treat-S-cursor-todo-selection-as-state-change nil ;; S-R, S-L skip note/log info when fixing state
      org-log-done 'time
      org-agenda-tags-column -130 ;; Set tags far to the right
      org-clock-out-remove-zero-time-clocks t ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      org-clock-persist t ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      org-use-fast-todo-selection t ;; from any todo state to any other state; using it keys
      org-agenda-window-setup 'only-window) ;; Always open my agenda in fullscreen

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
(add-hook 'org-agenda-mode-hook
          (lambda() (display-line-numbers-mode -1))) ;; Disable line numbers in org-agenda view

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

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up scheduled-down
                priority-down category-keep deadline-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

(setq org-agenda-block-separator ?\u2500) ;; use 'straight line' as a block-agenda divider

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
                                     "~/org/gtd/next.org"))))

          (todo "TODO"
                ((org-agenda-overriding-header "Inbox")
                 (org-agenda-files '("~/org/gtd/inbox.org"))))

          ;; Uncomment the following sections if needed
          ;; (todo "TODO"
          ;;       ((org-agenda-overriding-header "Emails")
          ;;        (org-agenda-files '("~/org/gtd/emails.org"))))

          ;; (todo "TODO"
          ;;       ((org-agenda-overriding-header "Projects")
          ;;        (org-agenda-files '("~/org/gtd/projects.org"))))

          (todo "TODO"
                ((org-agenda-overriding-header "One-off Tasks")
                 (org-agenda-files '("~/org/gtd/next.org"))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if
                                             'deadline 'scheduled))))
          nil))))

;; Clocking-out changes NEXT to HOLD
;; Clocking-in changes HOLD to NEXT
;; Original code: https://github.com/gjstein/emacs.d/blob/master/config/gs-org.el

(setq org-clock-in-switch-to-state 'zk/clock-in-to-next)
(setq org-clock-out-switch-to-state 'zk/clock-out-to-hold)

(defun zk/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
   Skips capture tasks, projects, and subprojects.
   Switch projects and subprojects from NEXT back to TODO."
  (unless (and (boundp 'org-capture-mode) org-capture-mode)
    (when (member (org-get-todo-state) '("TODO" "HOLD"))
      "NEXT")))

(defun zk/clock-out-to-hold (kw)
  "Switch a task from NEXT to HOLD when clocking out."
  (unless (and (boundp 'org-capture-mode) org-capture-mode)
    (when (member (org-get-todo-state) '("NEXT"))
      "HOLD")))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 48)
(setq org-habit-show-habits-only-for-today t)

;; Specify refile target using the file path
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

;; Confirm creating parent nodes
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Define refile targets
(setq org-refile-targets '(( "~/org/gtd/next.org" :level . 0) ; Next actions
                           ("~/org/ideas.org" :level . 1) ; Ideas
                           ("~/org/links.org" :level . 1) ; Links
                           ("~/org/gtd/someday.org" :regexp . "\\(?:\\(?:Task\\|idea\\|p\\(?:\\(?:os\\|rojec\\)t\\)\\)s\\)") ; Someday/Maybe
                           ("projects.org" :regexp . "\\(?:Tasks\\)"))) ; Projects

(defun zk/org-agenda-process-inbox-item ()
  (interactive)
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-schedule t)
   (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-refile nil nil t)))

(global-set-key (kbd "C-c o") 'zk/org-agenda-process-inbox-item)

(setq my/section-start-time (current-time))

(defun execute-shell-command (command)
  "Execute a shell command and print the output."
  (interactive "sShell command: ")
  (shell-command-to-string command))

(defun zk/start-local-server ()
  "Start the local web server for the blog."
  (interactive)
  (setq httpd-root "/home/zakaria/dox/blog/public/")
    (httpd-start)
    (message "Check out your blog on `localhost:8080`"))

(defun zk/prompt_for_blog_title()
  (read-string " Name of the Post: ")
  )

(defun zk/generate-blog-file ()
  "Create a new blog post. Prompts for the post name, generates a filename based on the date and post name, and inserts a template for the post."
  (interactive)
  (let* ((blog-post-title (read-string " Blog post title: "))          ; Prompt for the title of the blog post
         (blog-date-prefix (format-time-string "%Y-%m-%d"))             ; Get the current date in the format YYYY-MM-DD
         (blog-extension ".org")                                        ; Set the file extension as ".org"
         (blog-path "/home/zakaria/dox/blog/content/")                  ; Specify the directory where the blog files are stored
         (blog-filename (concat blog-path blog-date-prefix "-" (replace-regexp-in-string " " "-" (downcase blog-post-title)) blog-extension))
                                        ; Generate the filename based on the date and post title
         (blog-author "Zakaria.K")                                      ; Set the author name for the blog
         (blog-email "4.kebairia@gmail.com")                            ; Set the email for the blog
         (blog-html-options "html5-fancy:t tex:t")                      ; Set options for better HTML rendering
         (blog-org-startup-options "show2levels indent hidestars")      ; Set options for better Org mode rendering
         (blog-post-date (format-time-string "%d %B %Y"))               ; Format the current date for the blog post
         (blog-begin-date "#+begin_date\nDate: {{{date}}}")             ; Define the beginning tag for the date
         (blog-end-date "#+end_date\n"))                                ; Define the ending tag for the date

    ;; Create the file
    (if (file-exists-p blog-filename)
        (message (format "File '%s' already exists" blog-post-title))
      (with-current-buffer (find-file blog-filename)                    ; Open the file and switch to its buffer
        (insert
         (format
          "#+TITLE: %s
#+SUBTITLE:
#+AUTHOR: %s
#+EMAIL: %s
#+DATE: %s
#+OPTIONS: %s
#+STARTUP: %s
#+KEYWORDS:

"
          (capitalize blog-post-title)  
          blog-author                                                   
          blog-email                                                    
          blog-post-date                                                
          blog-html-options                                             
          blog-org-startup-options))))))

(defun zk/create-post ()
  (interactive)
  (progn
    (zk/generate-blog-file)
    (zk/start-local-server)))

;; get all the blog post filenames, excluding the index.org file
(defun zk/generate-list-of-blogs (path)
  (setq index-page "index.org")
  (remove index-page
  (mapcar 'file-name-nondirectory 
          (directory-files path t ".org" nil nil)
          )))
(setq blogs (zk/generate-list-of-blogs "/home/zakaria/dox/blog/content" ))

;; Calculate the number of blog posts
(setq number-of-blogs (length (zk/generate-list-of-blogs "/home/zakaria/dox/blog/content" )))

;; Get date for the a specific blog post
;; Extract blog post filename
;; 2023-04-06-building-a-homelab-with-kvm-and-kubernetes:-an-overview.org
;; 2023-04-11-building-qemu-kvm-images-with-packer-(part-I).org
(defun zk/blog-filename-to-html (filename)
  (string-replace  ".org" ".html" filename)
  )
(zk/blog-filename-to-html "2023-04-06-building-a-homelab-with-kvm-and-kubernetes:-an-overview.org")
;; Extract blog post Title
(defun zk/extract-blog-title (filename)
  ;; remove date
  ;; remove '-'
  (string-trim (capitalize
                (string-replace ".org" ""
                                (string-replace  "-" " "
                                                 (replace-regexp-in-string "[0-9]" "" filename))))))
(defun zk/extract-blog-publication-date (filename)
  (substring filename 0 10))

(zk/extract-blog-title "2023-12-12-building-a-homelab-with-kvm-and-kubernetes:-an-overview.org")
(zk/extract-blog-title "2023-04-11-building-qemu-kvm-images-with-packer-(part-I).org")

;; Generate items for rss file
;; ---------------------------
;; (lambda () (insert (concat "<title>" (zk/extract-blog-title) "</title>")))
(defun zk/generate-rss-title-item (filename)
  (concat "<title>" (zk/extract-blog-title filename) "</title>"))

(defun zk/generate-rss-pubdate-item (filename)
  (concat "<pubDate>" (zk/extract-blog-publication-date filename) "</pubDate>"))

(defun zk/generate-rss-link-item (filename)
  (concat "<link>" (zk/blog-filename-to-html filename) "</link>"))

(defun zk/generate-rss-item (filename)
  (print (format
          "<item>
%s
%s
%s
</item>

"
          (zk/generate-rss-pubdate-item filename)
          (zk/generate-rss-title-item filename)
          (zk/generate-rss-link-item filename)
          )))


(defun zk/generate-rss-content ()
  (dolist (item (zk/generate-list-of-blogs "/home/zakaria/dox/blog/content/"))
    (zk/generate-rss-item item)))
(print "------------------------------------------------------")


(defun zk/generate-rss-file (filename)
  "Generate an RSS file."
  (with-temp-file filename (concat 
                            "<?xml version='1.0' encoding='UTF-8'?>
<rss version='2.0'>
<channel>
<title>kebairia.github.io</title>
<link>https://kebairia.github.io</link>
<language>en-us</language>
<description>Articles and tutorials about open source, BSD and GNU/Linux system administration, and programming - the pragmatic way.</description>"
                            (zk/generate-rss-content)
                            "</channel></rss>"
                            )))
(zk/generate-rss-file "/tmp/rssl.xml")

(require 'transient)

(transient-define-prefix zk/blogging ()
  "Menu for my blogging activities"
  [["Blog post options"
    ("p" "Create a new post" zk/create-post)]
   ["Local blog server"
    ("s" "Start local blog" zk/start-local-server)]]
  )

;; Bind the transient menu to the key "C-c p"
(global-set-key (kbd "C-c p") 'zk/blogging)

(my/report-time "Blog")

(setq my/section-start-time (current-time))

(bind-key "C-c g" #'magit)

(setq magit-commit-show-diff nil)

(my/report-time "Versioning")

(setq my/section-start-time (current-time))

(require 'eglot)

(defun my-display-line-numbers-hook ()
  (display-line-numbers-mode 1)
  (setq display-line-numbers-width-start 1))

(add-hook 'prog-mode-hook 'my-display-line-numbers-hook)
(add-hook 'text-mode-hook 'my-display-line-numbers-hook)

(defun my-eglot-hook ()
  (eglot-ensure))

(add-hook 'python-mode-hook 'my-eglot-hook)
(add-hook 'sh-script-mode-hook 'my-eglot-hook)
;; (add-hook 'yaml-mode-hook 'my-eglot-hook)
(add-hook 'markdown-mode-hook 'my-eglot-hook)

;; Python server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(python-mode . ("pylsp" "-v" "--tcp" "--host"
                                "localhost" "--port" :autoport))))

;; Bash server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(sh-mode . ("bash-language-server" "start"))))

;; YAML server
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `(yaml-mode . ("yaml-language-server" "--stdio"))))

;; Markdown server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("marksman"))))

(my/report-time "IDE")

(let ((init-time (float-time (time-subtract (current-time) my/init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))

  (message "---------------------------------------------------------------")
  (message "Initialization time:                 %.2fs (+ %.2f system time)"
           init-time (- total-time init-time)))
  (message "---------------------------------------------------------------")
