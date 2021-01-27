(use-package jedi
 :ensure t
 :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
 )
(setq jedi:complete-on-dot t)

;; (add-to-list 'company-backends 'company-jedi)

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

(use-package flycheck
:ensure t
:init (global-flycheck-mode))

(use-package elpy
  :init
  :disabled t
  (elpy-enable))
