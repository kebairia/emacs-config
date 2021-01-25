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
;; load files
(load "~/.config/emacs/modules/DEFAULTS.el") 
(load "~/.config/emacs/modules/PACKAGES.el") 
(load "~/.config/emacs/modules/SESSIONS.el") 
(load "~/.config/emacs/modules/BINDINGS.el") 
(load "~/.config/emacs/modules/ORG.el") 
(load "~/.config/emacs/modules/MU4E.el") 
(load "~/.config/emacs/nano/nano.el") 
(load "~/.config/emacs/nano/nano-layout.el") 
(load "~/.config/emacs/nano/nano-modeline.el")

;; Measure emacs startup time
(add-to-list 'after-init-hook
             (lambda ()
               (message (concat "emacs ("
                                (number-to-string (emacs-pid))
                                ") started in "
                                (emacs-init-time)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-display-buffer-function 'magit-display-buffer-traditional)
 '(package-selected-packages
   '(org-mu4e zetteldeft yaml-mode which-key use-package undo-tree try sqlite3 smtpmail-multi sml-modeline smart-mode-line-powerline-theme rainbow-mode rainbow-delimiters projectile pbcopy ox-twbs ox-tufte ox-reveal ox-hugo outorg org-roam org-ref org-pdfview org-pdftools org-mime org-download org-cliplink ob-async notmuch mu4e-views mu4e-overview mpv minions memoize markdown-mode+ lsp-mode keyfreq jedi hl-todo helm-swoop helm-org gruvbox-theme graphviz-dot-mode fzf flycheck f3 evil-org evil-mu4e evil-magit evil-leader evil-collection eterm-256color ess emacsql-sqlite elpy elfeed-org dired-subtree dired-single counsel command-log-mode binder anki-editor anki-connect aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
