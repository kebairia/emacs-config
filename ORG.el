(setq org-agenda-directory "~/org/gtd/"
      org-agenda-files '("~/org/gtd" ))                 ;; org-agenda-files

 (setq org-agenda-dim-blocked-tasks nil                ;; Do not dim blocked tasks
       org-agenda-span 'day
       org-agenda-inhibit-startup t              ;; Stop preparing agenda buffers on startup:
       org-agenda-use-tag-inheritance nil              ;; Disable tag inheritance for agendas:
       org-agenda-show-log t
       org-agenda-skip-scheduled-if-done t
       org-agenda-skip-deadline-if-done t
       org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled

       org-agenda-time-grid
       '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "----------------"))
(setq
    org-agenda-start-on-weekday 0                       ;; Weekday start on Sunday
     org-treat-S-cursor-todo-selection-as-state-change nil;; S-R,S-L skip the note/log info[used when fixing the state]
      org-agenda-tags-column -100                     ;; Set tags far to the right
      org-clock-out-remove-zero-time-clocks t         ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      org-clock-persist t                             ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      org-use-fast-todo-selection t                   ;; from any todo state to any other state; using it keys
     org-agenda-window-setup 'only-window)              ;; Always open my agenda in fullscreen
(setq org-agenda-prefix-format
  '((agenda . " %i %-12:c%?-12t% s")
    (todo   . " ")
    (tags   . " %i %-12:c")
    (search . " %i %-12:c")))

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
(add-hook 'org-agenda-mode-hook                        ;; disable line-number when i open org-agenda view
           (lambda() (display-line-numbers-mode -1)))

;; (define-key global-map (kbd "C-c c") 'org-capture)
;; (define-key global-map (kbd "C-c a") 'org-agenda)

(setq org-agenda-block-separator 9472)     ; use 'straight line' as a block-agenda divider
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                ((org-agenda-overriding-header "Tasks")
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")

                   (org-agenda-files '("~/org/gtd/next.org"))
                   (org-deadline-warning-days 0)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                         ))

          (agenda nil
                  ((org-agenda-overriding-header "Deadlines")
                   (org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-prefix-format "  %?-12t% s")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))))

          (todo "TODO"
                     ((org-agenda-overriding-header "Inbox")
                      (org-agenda-files '("~/org/gtd/inbox.org"))
                      (org-agenda-prefix-format "  %?-12t% s")))

            ))))
          ;; (tags "CLOSED>=\"<today>\""
          ;;       ((org-agenda-overriding-header "Completed today")
          ;;        (org-agenda-prefix-format "  %?-12t% s")

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 48)
(setq org-habit-show-habits-only-for-today t)

;; Refiling [need reading]
(setq org-refile-use-outline-path 'file
 org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("~/org/gtd/next.org" :level . 0)
                           ("~/org/links.org" :level . 1)
                           ("~/org/ideas.org" :level . 1)
                           ("someday.org" :level . 0)
                           ("~/org/gtd/projects.org" :maxlevel . 2)))

(setq org-capture-templates
   `(("i" "Inbox" entry  (file "~/org/gtd/inbox.org")
    ,(concat "* TODO %?\n"
             "/Entered on/ %U"))))

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
