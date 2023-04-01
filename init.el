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
        evil                ; A VI layer for Emacs
        evil-collection
        evil-org
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
        magit               ; A Git porcelain inside Emacs.
        markdown-mode       ; Major mode for Markdown-formatted text
        use-package         ; A configuration macro for simplifying your .emacs
        vc-backup           ; VC backend for versioned backups
        yaml-mode           ; YAML mode
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

(bind-key "M-n"        #'my/make-frame)
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

(require 'minibuffer-header)

(setq minibuffer-header-show-message t
      minibuffer-header-hide-prompt t
      minibuffer-header-default-message "")

(set-face-attribute 'minibuffer-header-face nil
                    :inherit 'nano-subtle
                    :extend t)
(set-face-attribute 'minibuffer-header-message-face nil
                    :inherit '(nano-subtle nano-faded)
                    :extend t)

(defun my/minibuffer-header-format (prompt)
  "Minibuffer header"
  
  (let* ((prompt (replace-regexp-in-string "[: \t]*$" "" prompt))
         (depth (minibuffer-depth))
         (prompt (cond ((string= prompt "M-x") "Extended command")
                       ((string= prompt "Function") "Help on function")
                       ((string= prompt "Callable") "Help on function or macro")
                       ((string= prompt "Variable") "Help on variable")
                       ((string= prompt "Command") "Help on command")
                       ((string= prompt "Eval") "Evaluate lisp expression")
                       (t prompt))))
    (concat
     (propertize (format " %d " depth)
                 'face `(:inherit (nano-salient-i nano-strong)
                         :extend t))
     (propertize " "
                 'face 'nano-subtle 'display `(raise ,nano-modeline-space-top))

     (propertize prompt
                 'face `(:inherit (nano-subtle nano-strong nano-salient)
                         :extend t))
     (propertize " "
                 'face 'nano-subtle 'display `(raise ,nano-modeline-space-bottom))
     (propertize "\n" 'face 'highlight)
     (propertize " " 'face 'highlight
                     'display `(raise ,nano-modeline-space-top))
     (propertize "︎︎" 'face '(:inherit (nano-salient nano-strong)))
     (propertize " " 'face 'highlight
                     'display `(raise ,nano-modeline-space-bottom)))))

(setq minibuffer-header-format #'my/minibuffer-header-format)

(minibuffer-header-mode)

(defun my/minibuffer-setup ()

  (set-window-margins nil 0 0)
  (set-fringe-style '(0 . 0))
  (cursor-intangible-mode t)
  (face-remap-add-relative 'default :inherit 'highlight))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup)

;; Code from https://stackoverflow.com/questions/965263
(defun my/lookup-function (keymap func)
  (let ((all-bindings (where-is-internal (if (symbolp func)
                                             func
                                           (cl-first func))
                                         keymap))
        keys key-bindings)
    (dolist (binding all-bindings)
      (when (and (vectorp binding)
                 (integerp (aref binding 0)))
        (push binding key-bindings)))
    (push (mapconcat #'key-description key-bindings " or ") keys)
    (car keys)))


(defun my/minibuffer-show-last-command-setup ()
  (setq minibuffer-header-default-message
   (my/lookup-function (current-global-map) this-command)))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-show-last-command-setup)

(defun my/minibuffer-show-last-command-exit ()
  (setq minibuffer-header-default-message ""))
(add-hook 'minibuffer-exit-hook #'my/minibuffer-show-last-command-exit)

(defun my/vertico--resize-window (height)
  "Resize active minibuffer window to HEIGHT."
;;  (setq-local truncate-lines (< (point) (* 0.8 (vertico--window-width)))
    (setq-local truncate-lines t
                resize-mini-windows 'grow-only
                max-mini-window-height 1.0)
  (unless (frame-root-window-p (active-minibuffer-window))
    (unless vertico-resize
      (setq height (max height vertico-count)))
    (let* ((window-resize-pixelwise t)
           (dp (- (max (cdr (window-text-pixel-size))
                       (* (default-line-height) (1+ height)))
                  (window-pixel-height))))
      (when (or (and (> dp 0) (/= height 0))
                (and (< dp 0) (eq vertico-resize t)))
        (window-resize nil dp nil nil 'pixelwise)))))

(advice-add #'vertico--resize-window :override #'my/vertico--resize-window)

(setq minibuffer-prompt-properties '(read-only t
                                     cursor-intangible t
                                     face minibuffer-prompt)
      enable-recursive-minibuffers t)

(require 'mini-frame)

(defcustom my/minibuffer-position 'bottom
  "Minibuffer position, one of 'top or 'bottom"
  :type '(choice (const :tag "Top"    top)
                 (const :tag "Bottom" bottom))
  :group 'nano-minibuffer)


(defun my/minibuffer--frame-parameters ()
  "Compute minibuffer frame size and position."

  ;; Quite precise computation to align the minibuffer and the
  ;; modeline when they are both at top position
  (let* ((edges (window-pixel-edges)) ;; (left top right bottom)
         (body-edges (window-body-pixel-edges)) ;; (left top right bottom)
         (left (nth 0 edges)) ;; Take margins into account
         (top (nth 1 edges)) ;; Drop header line
         (right (nth 2 edges)) ;; Take margins into account
         (bottom (nth 3 body-edges)) ;; Drop header line
         (left (if (eq left-fringe-width 0)
                   left
                 (- left (frame-parameter nil 'left-fringe))))
         (right (nth 2 edges))
         (right (if (eq right-fringe-width 0)
                    right
                  (+ right (frame-parameter nil 'right-fringe))))
         (border 1)
         (width (- right left (* 1 border)))

         ;; Window divider mode
         (width (- width (if (and (bound-and-true-p window-divider-mode)
                                  (or (eq window-divider-default-places 'right-only)
                                      (eq window-divider-default-places t))
                                  (window-in-direction 'right (selected-window)))
                             window-divider-default-right-width
                           0)))
         (y (- top border)))

    (append `((left-fringe . 0)
              (right-fringe . 0)
              (user-position . t) 
              (foreground-color . ,(face-foreground 'highlight nil 'default))
              (background-color . ,(face-background 'highlight nil 'default)))
            (cond ((and (eq my/minibuffer-position 'bottom))
                   `((top . -1)
                     (left . 0)
                     (width . 1.0)
                     (child-frame-border-width . 0)
                     (internal-border-width . 0)))
                  (t
                   `((left . ,(- left border))
                     (top . ,y)

                     (width . (text-pixels . ,width))
                     (child-frame-border-width . ,border)
                     (internal-border-width . 0)))))))

  (set-face-background 'child-frame-border (face-foreground 'nano-faded))
  (setq mini-frame-default-height 3)
  (setq mini-frame-create-lazy t)
  (setq mini-frame-show-parameters 'my/minibuffer--frame-parameters)
  (setq mini-frame-ignore-commands
        '("edebug-eval-expression" debugger-eval-expression))
  (setq mini-frame-internal-border-color (face-foreground 'nano-faded))

  (setq mini-frame-resize-min-height 3)
  (setq mini-frame-resize t)
  ;; (setq mini-frame-resize 'grow-only)
  ;; (setq mini-frame-default-height (+ 1 vertico-count))
  ;; (setq mini-frame-resize-height (+ 1 vertico-count))
  ;; (setq mini-frame-resize nil)

;; (mini-frame-mode 1)

(defun my/mini-frame--resize-mini-frame (frame)
  "Resize FRAME vertically only.
This function used as value for `resize-mini-frames' variable."
  (funcall mini-frame--fit-frame-function
           frame
           mini-frame-resize-max-height
           (if (eq mini-frame-resize 'grow-only)
               (max (frame-parameter frame 'height)
                    mini-frame-resize-min-height)
             mini-frame-resize-min-height)
           ;; A max-width must be included to work around a bug in Emacs which
           ;; causes wrapping to not be taken into account in some situations
           ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=56102
           (window-body-width)
           nil
           'vertically)

  (if (eq my/minibuffer-position 'top)
      (modify-frame-parameters  mini-frame-completions-frame `((top . 0)))
    (modify-frame-parameters  mini-frame-completions-frame `((top . (- 1))))))

(my/report-time "Minibuffer/Modeline")
