;; ;; Adding a separator line between days in Emacs Org-mode calender view (prettier)

;;     (setq org-agenda-format-date (lambda (date) (concat "\n"
;;                                                         (make-string (window-width) 9472)
;;                                                         "\n"
;;                                                         (org-agenda-format-date-aligned date))))
(setq org-agenda-directory "~/org/gtd/"
      org-agenda-files '("~/org/gtd" ))                    ;; org-agenda-files

(setq org-agenda-dim-blocked-tasks nil                    ;; Do not dim blocked tasks
      org-agenda-span 'day                                ;; show me one day
      org-agenda-inhibit-startup t                        ;; Stop preparing agenda buffers on startup:
      org-agenda-use-tag-inheritance nil                  ;; Disable tag inheritance for agendas:
      org-agenda-show-log t
      ;;org-agenda-skip-scheduled-if-done t
      ;;org-agenda-skip-deadline-if-done t
      ;;org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-skip-scheduled-if-deadline-is-shown t     ;; skip scheduled if they are already shown as a deadline
      org-agenda-deadline-leaders '("!D!: " "D%2d: " "")
      org-agenda-scheduled-leaders '("" "S%3d: ")

      org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "----------------"))
(setq
 org-agenda-start-on-weekday 0                          ;; Weekday start on Sunday
 org-treat-S-cursor-todo-selection-as-state-change nil ;; S-R,S-L skip the note/log info[used when fixing the state]
 org-log-done 'time
 org-agenda-tags-column -130                          ;; Set tags far to the right
 org-clock-out-remove-zero-time-clocks t              ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
 org-clock-persist t                                  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
 org-use-fast-todo-selection t                        ;; from any todo state to any other state; using it keys
 org-agenda-window-setup 'only-window)                 ;; Always open my agenda in fullscreen

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t %s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))
;; define org's states
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
;; sort my org-agenda preview
(setq org-agenda-sorting-strategy '((agenda habit-down
                                            time-up
                                            scheduled-down
                                            priority-down
                                            category-keep
                                            deadline-down)
                                    (todo priority-down category-keep)
                                    (tags priority-down category-keep)
                                    (search category-keep)))

;;Thanks to Erik Anderson, we can also add a hook that will log when we activate
;;a task by creating an “ACTIVATED” property the first time the task enters the NEXT state:
(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
(add-hook 'org-agenda-mode-hook                            ;; disable line-number when i open org-agenda view
           (lambda() (display-line-numbers-mode -1)))

;; (define-key global-map (kbd "C-c c") 'org-capture)
;; (define-key global-map (kbd "C-c a") 'org-agenda)

(setq org-agenda-block-separator  9472)                  ;; use 'straight line' as a block-agenda divider
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-deadline-warning-days 365)))

          (todo "NEXT"
                ((org-agenda-overriding-header "In Progress")
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-files '("~/org/gtd/someday.org"
                                     "~/org/gtd/projects.org"
                                     "~/org/gtd/next.org"))
                 ))
          (todo "TODO"
                ((org-agenda-overriding-header "inbox")
                 (org-agenda-files '("~/org/gtd/inbox.org"))))

          (todo "TODO"
                ((org-agenda-overriding-header "Emails")
                 (org-agenda-files '("~/org/gtd/emails.org"))))

          (todo "TODO"
                ((org-agenda-overriding-header "Projects")
                 (org-agenda-files '("~/org/gtd/projects.org")))
                )

          (todo "TODO"
                ((org-agenda-overriding-header "One-off Tasks")
                 (org-agenda-files '("~/org/gtd/next.org"))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if
                                             'deadline 'scheduled))))
          nil))))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 48)
(setq org-habit-show-habits-only-for-today t)

;; Refiling [need reading]
;;tell org-mode we want to specify a refile target using the file path.
(setq org-refile-use-outline-path 'file
 org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("~/org/gtd/next.org" :level . 0)
                           ("~/org/ideas.org" :level . 1)
                           ("~/org/links.org" :level . 1)
                           ("~/org/gtd/someday.org" :regexp . "\\(?:\\(?:Task\\|idea\\|p\\(?:\\(?:os\\|rojec\\)t\\)\\)s\\)")
                           ("projects.org" :regexp . "\\(?:Tasks\\)"))) 
;;("someday.org" :level . 0)

(setq org-capture-templates
      `(("i" "Inbox" entry  (file "~/org/gtd/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
        ("l" "Link" entry (file+headline "~/org/gtd/inbox.org" "Links")
         ,(concat "* TODO %a %?\n"
                  "/Entered on/ %U") :immediate-finish t)
        ("e" "email" entry (file+headline "~/org/gtd/emails.org" "Emails")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")

        ;; ("m" "mood" entry (file "~/org/mood.org" )
        ;;  ,(concat "* %? \n %^{MOOD} \n"
        ;;           "/Entered on/ %U") :immediate-finish t)
        ))

(require 'org-protocol)

(setq reftex-default-bibliography '("~/org/ref/org-ref.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "/tmp/test/notes.org"
      org-ref-default-bibliography '("~/org/ref/org-ref.bib")
      org-ref-pdf-directory "~/org/ref/pdfs")

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "CANCELED")))
(setq org-todo-keyword-faces
  '(
    ("TODO" . (:foreground "brown2" :weight bold))
    ("READ" . (:foreground "brown2" :weight bold))

    ("NEXT" . (:foreground "#00b0d1"  :weight bold ))
    ("READING" . (:foreground "#00b0d1"  :weight bold ))

    ("DONE" . (:foreground "#16a637" :weight bold))

    ("HOLD" . (:foreground "orange"  :weight bold))

    ("CANCELED" . (:foreground "gray" :background "red1" :weight bold))
  ))

(use-package ox-reveal
  :ensure ox-reveal)
(setq org-reveal-root
      "file:///home/zakaria/org/conf/revealJS/reveal.js-4.1.0")
(setq org-reveal-mathjax t)

(eval-after-load "org"
  (use-package ob-async
    :ensure t
    :init (require 'ob-async)))
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-tab-acts-natively t)
;; (require 'org-tempo)
;; (add-to-list 'org-structure-template-alist '("s" . "src sh"))
;; (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;; (add-to-list 'org-structure-template-alist '("p" . "src python"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)
   (R . t)
   ))

(defun zk/switch-to-agenda ()
     (interactive)
     (org-agenda nil "g"))
;; PS: check out the original code from here:
;; https://github.com/gjstein/emacs.d/blob/master/config/gs-org.el

;;clocking-out changes NEXT to HOLD
;;clocking-in changes HOLD to NEXT
(setq org-clock-in-switch-to-state 'zk/clock-in-to-next)
(setq org-clock-out-switch-to-state 'zk/clock-out-to-hold)
(defun zk/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
   Skips capture tasks, projects, and subprojects.
   Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO")))
      "NEXT")
     ((and (member (org-get-todo-state) (list "HOLD")))
      "NEXT")
      )))
(defun zk/clock-out-to-hold (kw)
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "NEXT")))  "HOLD")
      )))

(add-hook 'org-mode-hook 'org-indent-mode)
;; use '⤵' instead of '...' in headlines
;;(setq org-ellipsis "⤵")

(use-package org-appear
 :load-path "~/.config/emacs/modules/org-appear/")
(add-hook 'org-mode-hook 'org-appear-mode)
(setq
 org-appear-autolinks t
 org-appear-autosubmarkers t)
