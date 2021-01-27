;; some shortcuts -- files
(global-set-key (kbd "C-c C") (lambda() (interactive)(find-file "~/.config/emacs/init.org")))
(global-set-key (kbd "C-c b") (lambda() (interactive)(find-file "~/org/books.org")))
(global-set-key (kbd "C-c I") (lambda() (interactive)(find-file "~/org/gtd/inbox.org")))
;;(global-set-key (kbd "C-c L") (lambda() (interactive)(find-file "~/org/links.org")))
;;(global-set-key (kbd "C-c E") (lambda() (interactive)(find-file "~/org/gtd/emails.org")))
(global-set-key (kbd "<f12>") (lambda() (interactive)(find-file "~/org/files/org.pdf")))
;; Reload buffer with <F5>
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

(defun zk/split-go-right()
  (interactive)
  (split-window-horizontally)
  (windmove-right))
(defun zk/split-go-down()
  (interactive)
  (split-window-vertically)
  (windmove-down))
;; try to go to the other window automaticly
(global-set-key (kbd "C-x i") 'zk/split-go-right)
(global-set-key (kbd "C-x m") 'zk/split-go-down)

;; Move between buffer
(global-set-key (kbd "M-n") 'switch-to-next-buffer)
(global-set-key (kbd "M-p") 'switch-to-prev-buffer)

;; Move between Windows
(global-set-key (kbd "C-x k") 'windmove-up)
(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x l") 'windmove-right)
(global-set-key (kbd "C-x h") 'windmove-left)

;; Resize windows
(global-set-key (kbd "C-M-l") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-h") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-j") 'shrink-window)
(global-set-key (kbd "C-M-k") 'enlarge-window)

(global-set-key (kbd "M-o") 'delete-other-windows)
(global-set-key (kbd "C-x p") 'zk/org-agenda-process-inbox-item)
