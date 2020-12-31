;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

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
