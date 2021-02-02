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
         "w" 'org-agenda-week-view
         "m" 'org-agenda-month-view
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
         ;; "f" 'pdf-links-action-perform
         ;; "b" 'ibuffer
         ;; "n" 'org-noter
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
(org-roam-mode 1)

(use-package magit)
(use-package evil-magit
  :after magit)
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

;; it looks like counsel is a requirement for swiper
;; counsel give us a nice looking interface when we use M-x
(use-package counsel
  :ensure t)
(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    ;(global-set-key "\C-i" 'counsel-org-goto-all)
    (global-set-key (kbd "\C-c g") 'counsel-git)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "\C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "\C-c j") 'counsel-git-grep)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(global-aggressive-indent-mode 1)
