(straight-use-package 'use-package)
;; make `use-package` to automatically install all of your packages 
;; without the need for adding `:straight t`.
(setq straight-use-package-by-default t)

(straight-use-package 'org)

(straight-use-package 'evil)
(setq evil-want-keybinding nil)                   ;; this statement is required to enable evil/evil-colleciton mode
(evil-mode 1)
(setq evil-want-abbrev-expand-on-insert-exit nil)

(straight-use-package
 '(evil-collection
   :type git
   :host github :repo "emacs-evil/evil-collection"
   :after evil
   :config
   (evil-collection-init)))

;; (straight-use-package 'evil-collection
;;                       :after evil
;;                       :config
;;                       (evil-collection-init))

(straight-use-package 'evil-leader)

(straight-use-package 'magit)

(straight-use-package 'undo-tree)

(straight-use-package 'which-key)

(straight-use-package 'ibuffer)

(straight-use-package 'selectrum)

(straight-use-package 'ctrlf)

(straight-use-package 'aggressive-indent)

(straight-use-package 'pdf-tools)
