;; Better Defaults
;;----------------
;;(add-to-list
;; 'default-frame-alist '(fullscreen . fullboth)) ; Start emacs/emacsclient in fullscreen mode
(require 'generic-x)                        ; Add syntax high' for ini/conf/batch files
(setq-default
    fill-column 80                          ; Set width for automatic line breaks
    tab-width 4                             ; Set width for tabs
    indent-tabs-mode nil)                   ; Stop using tabs to indent
(setq
    tab-always-indent 'complete)             ; Tab always indent
(electric-pair-mode 1)
(global-hl-line-mode t)                     ; highlight current line
(fset 'yes-or-no-p 'y-or-n-p)               ; Replace yes/no prompts with y/n

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

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
