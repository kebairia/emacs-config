;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

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
