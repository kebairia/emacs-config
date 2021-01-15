;;; package --- Summary  
;; Load configuration from ~/.config/emacs/modules/*.el

         ;;; Commentary:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; Speed up startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)
;; package managers
(require 'package)
;;(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; emacs 27 now does'nt need a package-initialize
(when (< emacs-major-version 27)
  (package-initialize))

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

(load-user-file "nano/nano.el")
;;(load-user-file "nano/nano-session.el")
;(load-user-file "nano/nano-base-colors.el")
;(load-user-file "nano/nano-faces.el")
;(load-user-file "nano/nano-theme-dark.el")
;(load-user-file "nano/nano-theme.el")

(load-user-file "modules/DEFAULTS.el")
(load-user-file "modules/SESSIONS.el")
(load-user-file "modules/BINDINGS.el")
;;(load-user-file "modules/THEME.el")
(load-user-file "modules/PACKAGES.el")
(load-user-file "modules/ORG.el")
;; Measure emacs startup time
(add-to-list 'after-init-hook
             (lambda ()
               (message (concat "emacs (" (number-to-string (emacs-pid)) ") started in " (emacs-init-time)))))
