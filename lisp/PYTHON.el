(use-package jedi
 :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))
(setq jedi:complete-on-dot t)

(use-package flycheck
:init (global-flycheck-mode))

(use-package elpy
  :init
  (elpy-enable))
;; (setq elpy-rpc-backend "jedi")
