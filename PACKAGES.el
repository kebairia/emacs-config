(use-package gruvbox-theme
 :init )
 ;; the bellow is used so that emacs will trust the elisp code[the theme]
 ;; in future
(custom-set-variables
 '(custom-enabled-themes  '(gruvbox-dark-hard))
 '(custom-safe-themes
    '("4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb"
       default)))

(setq evil-want-keybinding nil)                   ;; this statement is required to enable evil/evil-colleciton mode
(evil-mode 1)                                     ;; enable evil-mode
(setq evil-want-abbrev-expand-on-insert-exit nil)
(use-package evil-collection                      ;; evil-friendly binding for many modes
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; (use-package evil-magit
;;   :after evil)
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
         "a" 'zk/switch-to-agenda
         "w" 'org-agenda-week-view
         "m" 'org-agenda-month-view
         ;; "d" 'deft
         ;; "I" 'zetteldeft-insert-list-links
         ;; "N" 'zetteldeft-new-file-and-link
         ;; "B" 'zetteldeft-new-file-and-backlink
         ;; "f" 'pdf-links-action-perform
         ;; "b" 'ibuffer
         ;; "t" 'term
         "c" 'org-capture
         ;; "g" 'magit-status
         "r" 'bookmark-bmenu-list
         "l" 'org-store-link
         "L" 'org-insert-link
         ;; "n" 'org-noter
         "q" 'kill-current-buffer)
        (evil-leader-mode 1)
        (global-evil-leader-mode 1))
         ;;"B" 'zetteldeft-backlink-add
         ;;"s" 'zk/gen-scratch-buffer

;;Use minions to hide all minor modes
(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

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

(global-set-key (kbd "C-x o") 'ace-window)

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key "\M-S" 'counsel-org-goto)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))
