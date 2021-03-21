(setq evil-want-keybinding nil)                   ;; this statement is required to enable evil/evil-colleciton mode
(evil-mode 1)                                     ;; enable evil-mode
(setq evil-want-abbrev-expand-on-insert-exit nil)
(use-package evil-collection                      ;; evil-friendly binding for many modes
  :after evil
  :ensure t
  :config
  (evil-collection-init))
(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(setq                                             ;;automatically use evil for ibuffer and dired
 evil-emacs-state-modes
    (delq 'ibuffer-mode evil-emacs-state-modes))

(use-package evil-leader
;; needs to be enabled before M-x evil-mode!
    :ensure t
    :config
        (evil-leader/set-leader ",")
        (evil-leader/set-key
         "e" 'mu4e
         "a" 'zk/switch-to-agenda
         "d" 'deft
         "g" 'magit-status
         "i" 'org-roam-insert
         "I" 'org-roam-insert-immediate
         "f" 'org-roam-find-file
         "l" 'org-roam
         "t" 'term
         "c" 'org-capture
         "r" 'counsel-recentf
         "b" 'bookmark-bmenu-list
         "L" 'org-insert-link
         "q" 'kill-current-buffer)
         ;;"l" 'org-store-link
         ;; "B" 'zetteldeft-new-file-and-backlink
         "F" 'pdf-links-action-perform
         ;; "b" 'ibuffer
         "n" 'org-noter
        (evil-leader-mode 1)
        (global-evil-leader-mode 1))
         ;;"B" 'zetteldeft-backlink-add
         ;;"s" 'zk/gen-scratch-buffer

;; disable linum-mode (line number)
(add-hook 'deft
'(lambda () (linum-mode nil)))
 (use-package deft
    :commands (deft)
    :custom       (deft-directory "~/org/notes" )
                  (deft-recursive t)
                  (deft-extensions '("org" "md" "txt") )
                  (deft-use-filename-as-title t)
                  (deft-file-naming-rules
                    '((noslash . "-")
                      (nospace . "-")
                      (case-fn . downcase))
                  deft-org-mode-title-prefix t
                  deft-text-mode 'org-mode))

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "/home/zakaria/org/notes")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))
;; (setq org-roam-completion-system 'ivy)
(org-roam-mode 1)

(use-package magit)
;;(use-package evil-magit
;;  :after magit)
"Display BUFFER in same-window"
(custom-set-variables
 '(magit-display-buffer-function 'magit-display-buffer-traditional))
;; '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;;turn on everywhere
(global-undo-tree-mode 1)
;; Save history to a file
(setq
    undo-tree-auto-save-history 1 ; Show relative times in the undo tree visualizer
    undo-tree-visualizer-timestamps 1; Show diffs when browsing through the undo tree
    undo-tree-visualizer-diff 1)

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

(use-package which-key
 :ensure t
 :config
 (which-key-mode))

(use-package selectrum
  :ensure t)
(selectrum-mode +1)
;; to make sorting and filtering more intelligent
(selectrum-prescient-mode +1)

;; to save your command history on disk, so the sorting gets more
;; intelligent over time
(prescient-persist-mode +1)
;; In Emacs 27 there is also a flex style which you might like.
(setq completion-styles '(substring partial-completion))
(setq selectrum-show-indices t)

(use-package ctrlf
  :ensure t)
(ctrlf-mode +1)

(global-aggressive-indent-mode 1)

(use-package pdf-tools
:ensure t
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

(use-package org-pdfview
:ensure t)
;; Set the pdf-view incompatible-modes[linum mode: line numbers]
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))

;; org noter configuration
(use-package org-noter
 :after org
 :ensure t
 :config (setq org-noter-auto-save-last-location t
               org-noter-doc-split-fraction (quote (0.7 . 0.7))
               org-noter-notes-window-behavior nil
               org-noter-always-create-frame nil
               org-noter-separate-notes-from-heading t))
