;;; package --- SummarY
;; Load configuration from ~/.config/emacs/modules/*.el

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

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.config/emacs")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;;(load-user-file "DEFAULTS.el")
(load-user-file "modules/KEYBIDING.el")
(load-user-file "modules/PACKAGES.el")
(load-user-file "nano/nano.el")
(load-user-file "nano/nano-base-colors.el")
(load-user-file "nano/nano-faces.el")
(load-user-file "nano/nano-theme-dark.el")
(load-user-file "nano/nano-theme.el")

;; (load-user-file "clean-theme/clean.el")
;; (load-user-file "clean-theme/defaults.el")
(load-user-file "modules/ORG.el")

;; Hist, Backup and auto-save ..etc
;;---------------------------------
(setq backup-directory-alist '(("." . "~/.cache/emacs/backup"))
  backup-by-copying t                       ; Don't delink hardlinks
  version-control t                         ; Use version numbers on backups
  delete-old-versions t                     ; Automatically delete excess backups
  kept-new-versions 3                       ; how many of the newest versions to keep
  kept-old-versions 3                       ; and how many of the old
  vc-make-backup-files t                    ; Even version controlled files get to be backed up.
  )
(setq auto-save-file-name-transforms
      '((".*" "~/.cache/emacs/undo/" t)))
(setq undo-tree-history-directory-alist     ; Saving persistent tree-undo to a single directory
      '(("." . "~/.cache/emacs/undo")))

 (setq is-work nil)                         ; *--=~~ search for explanations ~~=--*
 ;; t means no truncation
 (setq history-length t)
 (setq history-delete-duplicates t)

 (savehist-mode 1)                           ; Saves your minibuffer histories
 (setq
     savehist-file "~/.cache/emacs/savehist")    ; Set the savehist file
 (setq savehist-save-minibuffer-history 1)
 (setq savehist-additional-variables         ; Save other histories and other variables as well
     '(kill-ring
       search-ring
       regexp-search-ring))
