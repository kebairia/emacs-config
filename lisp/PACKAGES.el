(use-package org
  :hook (org-mode . enable-writing-minor-modes))

(use-package bookmark+)

(use-package visual-fill-column)
(add-hook 'visual-fill-column-mode-hook #'visual-line-mode)

(straight-use-package
 '(secret-mode
   :type git
   :host github :repo "/bkaestner/secret-mode.el"))

(setq evil-want-keybinding nil)                   
;; put this before loading evil to work
(setq evil-want-C-i-jump nil)
(straight-use-package 'evil)
;; this statement is required to enable evil/evil-colleciton mode
(evil-mode 1)
(setq evil-want-abbrev-expand-on-insert-exit nil)

;; after evil
(straight-use-package
 '(evil-collection
   :type git
   :host github :repo "emacs-evil/evil-collection"))
(evil-collection-init)

(straight-use-package '(evil-org-mode
                        :type git
                        :host github
                        :repo "Somelauw/evil-org-mode"))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
;; config

;; (add-hook 'org-mode-hook 'evil-org-mode)
;; (add-hook 'evil-org-mode-hook
;;           (lambda () (evil-org-set-key-theme)))
;; (require 'evil-org-agenda)
;; (evil-org-agenda-set-keys)
;; (setq                                             ;;automatically use evil for ibuffer and dired
;; evil-emacs-state-modes
;; (delq 'ibuffer-mode evil-emacs-state-modes))

(straight-use-package 'evil-leader)
;; needs to be enabled before M-x evil-mode!
;; :config
(evil-leader-mode 1)
(global-evil-leader-mode 1)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'org-export-dispatch
  "a" 'zk/switch-to-agenda
  "d" 'deft
  "g" 'magit-status
  "i" 'org-roam-node-insert
  "f" 'org-roam-capture
  "D" 'org-roam-dailies-capture-today
  "l" 'org-roam-buffer-toggle
  "z" 'term
  "c" 'org-capture
  "b" 'bookmark-jump
  "L" 'org-insert-link
  "q" 'kill-current-buffer
  "F" 'pdf-links-action-perform
  "s" 'secret-mode
  "n" 'org-noter
  "m i" 'org-noter-insert-note
  "m p" 'org-noter-insert-precise-note
  "m k" 'org-noter-sync-prev-note
  "m j" 'org-noter-sync-next-note
  "m s" 'org-noter-create-skeleton
  "m q" 'org-noter-kill-session
  "r c" 'org-ref-clean-bibtex-entry
  "r s" 'org-ref-bibtex-sort-order
  "r b" 'org-ref-bibliography
  "r g" 'org-ref-add-glossary-entry
  "r a" 'org-ref-add-acronym-entry
)
;; "r" 'consult-recent-file
;;"l" 'org-store-link
;;"s" 'zk/gen-scratch-buffer
;; )

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
;; '(magit-display-buffer-function 'magit-display-buffer-traditional))
(straight-use-package 'evil-magit)

(use-package projectile
:config (projectile-mode)
:bind-keymap
("C-c p" . projectile-command-map)
:init
(when (file-directory-p "~/dox/wrk")
  (setq projectile-project-search-path '("~/dox/wrk" "~/dox/wrk/pfe" ))))
;; speed up projectile by enabling caching
(setq projectile-enable-caching t)

(use-package undo-tree
  ;;turn on everywhere
  :init (global-undo-tree-mode 1))

(straight-use-package 'aggressive-indent)

(global-aggressive-indent-mode 1)

(straight-use-package 'ibuffer)
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

(straight-use-package 'which-key)
(which-key-mode)
(setq which-key-popup-type 'minibuffer)
;; (which-key-setup-side-window-right)

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))
;; disable marginalia by default
(defun marginalia-use-builtin ()
  (interactive)
  (mapc
   (lambda (x)
     (setcdr x (cons 'none (remq 'builtin (cdr x)))))
   marginalia-annotator-registry))
(marginalia-use-builtin)

(straight-use-package 'selectrum)
(selectrum-mode +1)
;; to make sorting and filtering more intelligent
(straight-use-package 'selectrum-prescient)
(selectrum-prescient-mode +1)

;; to save your command history on disk, so the sorting gets more
;; intelligent over time
(prescient-persist-mode +1)
;; ;; In Emacs 27 there is also a flex style which you might like.
;; (setq completion-styles '(substring partial-completion))
;;(setq selectrum-show-indices nil)

(straight-use-package 'ctrlf)
(setq ctrlf-default-search-style 'fuzzy-regexp)
(setq ctrlf-auto-recenter 1)
(setq ctrlf-highlight-line 1)

(ctrlf-mode +1)

(straight-use-package 'consult)

(use-package pdf-tools
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
;; dark mode 
(setq pdf-view-midnight-colors '("#f8f8f2" . "#1d2021"))

(use-package org-pdfview)
;; Set the pdf-view incompatible-modes[linum mode: line numbers]
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))

(use-package org-noter
  :after org
  :config
  (setq org-noter-auto-save-last-location t
        org-noter-doc-split-fraction (quote (0.7 . 0.7))
        org-noter-notes-window-behavior nil
        org-noter-notes-window-location "Vertical"
        org-noter-always-create-frame nil
        org-noter-separate-notes-from-heading t)
   )

(use-package eterm-256color)

(straight-use-package 'yaml-mode)

(straight-use-package 'dockerfile-mode)

(use-package elfeed)
(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.config/elfeed/elfeed.org")))
(global-set-key (kbd "C-x w") 'elfeed)
