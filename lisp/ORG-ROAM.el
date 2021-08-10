(use-package org-roam
  :ensure t
  ;; use org-roam v2
  :init 
  (setq org-roam-v2-ack t)
  (org-roam-setup)
  :custom
  (org-roam-directory (file-truename "/home/zakaria/dox/braindump/org-files"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  )
;;Configuring the Org-roam buffer display
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
;; Garbage Collection
(setq org-roam-db-gc-threshold most-positive-fixnum)

;; winner mode
(winner-mode +1)
(define-key winner-mode-map (kbd "<M-left>") #'winner-undo)
(define-key winner-mode-map (kbd "<M-right>") #'winner-redo)

(setq org-roam-graph-viewer
      (lambda (file)
        (let ((org-roam-graph-viewer "/usr/bin/brave"))
          (org-roam-graph--open (concat "file://///" file)))))

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
