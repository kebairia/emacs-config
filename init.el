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
;; use use-package along with straight
(straight-use-package 'use-package)
;; make `use-package` to automatically install all of your packages 
;; without the need for adding `:straight t`.
(setq straight-use-package-by-default t)

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

;; load files
(load "~/.config/emacs/modules/DEFAULTS.el") 
(load "~/.config/emacs/modules/PACKAGES.el") 
(load "~/.config/emacs/modules/SESSIONS.el") 
(load "~/.config/emacs/modules/BINDINGS.el") 
(load "~/.config/emacs/modules/ORG.el") 
;; (load "~/.config/emacs/modules/ORG-ROAM.el") 
                                        ;(load "~/.config/emacs/modules/MU4E.el") 
(load "~/.config/emacs/nano/nano.el") 	;
(load "~/.config/emacs/nano/nano-layout.el") 
(load "~/.config/emacs/nano/nano-modeline.el") 
;; (add-to-list 'load-path "~/.config/emacs/emacs-reveal")
;; (require 'emacs-reveal)

;; Measure emacs startup time
(add-to-list 'after-init-hook
             (lambda ()
               (message (concat "emacs ("
                                (number-to-string (emacs-pid))
                                ") started in "
                                (emacs-init-time)))))
