(straight-use-package 'org)

(straight-use-package 'evil)
;; this statement is required to enable evil/evil-colleciton mode
(setq evil-want-keybinding nil)                   
(evil-mode 1)
(setq evil-want-abbrev-expand-on-insert-exit nil)

;; after evil
(straight-use-package '(evil-collection
                        :type git
                        :host github :repo "emacs-evil/evil-collection"))
(evil-collection-init)

(straight-use-package 'evil-org)
;; config

(add-hook 'org-mode-hook 'evil-org-mode)
(add-hook 'evil-org-mode-hook
          (lambda () (evil-org-set-key-theme)))
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
                        "e" 'mu4e
                        "a" 'zk/switch-to-agenda
                        "d" 'deft
                        "g" 'magit-status
                        "i" 'org-roam-insert
                        "I" 'org-roam-insert-immediate
                        "f" 'org-roam-capture
                        "D" 'org-roam-dailies-capture-today
                        "l" 'org-roam
                        "t" 'term
                        "c" 'org-capture
                        "r" 'consult-recent-file
                        "b" 'bookmark-bmenu-list
                        "L" 'org-insert-link
                        "q" 'kill-current-buffer
                        "F" 'pdf-links-action-perform
                        "s" 'zk/gen-scratch-buffer
                        "n" 'org-noter)
;; "b" 'ibuffer
;;"l" 'org-store-link
;; "B" 'zetteldeft-new-file-and-backlink
;;"B" 'zetteldeft-backlink-add
;;"s" 'zk/gen-scratch-buffer
;; )

(straight-use-package 'magit
                      ;;(use-package evil-magit
                      ;;  :after magit)
                      "Display BUFFER in same-window"
                      (custom-set-variables
                       '(magit-display-buffer-function 'magit-display-buffer-traditional))
                      ;; '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
                      )

(straight-use-package 'which-key)
(which-key-mode)

(straight-use-package 'undo-tree)

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
         ("Elisp"  (name . "^.*el"))
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
(ctrlf-mode +1)

(straight-use-package 'aggressive-indent)

(global-aggressive-indent-mode 1)

(straight-use-package 'pdf-tools)
;; config
   (pdf-tools-install)
    ;; open pdfs scaled to fit page
   (setq-default pdf-view-display-size 'fit-page)
    ;; exchange isearch -- occur, occur -- isearch
   (define-key pdf-view-mode-map (kbd "C-s") 'occur)
   (define-key pdf-view-mode-map (kbd "M-s o") 'isearch-forward)
   ;; turn off cua so copy works
   (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
   ;; more fine-grained zooming
   (setq pdf-view-resize-factor 1.1)
