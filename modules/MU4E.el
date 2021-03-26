(require 'org-mime)
(use-package mu4e
  :ensure nil
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  ;;(setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a"
        ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
        mu4e-view-prefer-html t
        mu4e-update-interval 180
        mu4e-headers-auto-update t
        mu4e-compose-signature-auto-include t
        mu4e-compose-signature 
        (concat "Kebairia Zakaria\n"
                "Github: www.github.com/kebairia\n"
                "linkedIn: www.linkedin.com/in/zakaria-kebairia\n")
        mu4e-compose-format-flowed t ;; Make sure plain text mails flow correctly for recipients
        )

  (setq mu4e-maildir (expand-file-name "~/.local/share/mail"))

  (setq mu4e-contexts
        (list
         ;; ESI account
         (make-mu4e-context
          :name "esi"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/ESI" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "z.kebairia@esi-sba.dz")
                  (user-full-name    . "Kebairia Zakaria, ISI, G03")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . tls)
                  (smtpmail-auth-credentials . (expand-file-name "~/.local/share/emacs/authinfo"))
                  (mu4e-drafts-folder  . "/z.kebairia@esi-sba.dz/[Gmail].Drafts")
                  (mu4e-sent-folder  . "/z.kebairia@esi-sba.dz/[Gmail].Sent Mail")
                  (mu4e-refile-folder  . "/z.kebairia@esi-sba.dz/[Gmail].All Mail")
                  (mu4e-trash-folder  . "/z.kebairia@esi-sba.dz/[Gmail].Trash")

                  ))
         ;; Personal account
         (make-mu4e-context
          :name "Personal"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/4.kebairia" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "4.kebairia@gmail.com")
                  (user-full-name    . "Kebairia Zakaria")
                  (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . tls)
                  (smtpmail-auth-credentials . (expand-file-name "~/.local/share/emacs/authinfo"))
                  (smtpmail-debug-info . t)
                  (smtpmail-debug-verbose . t)
                  (mu4e-drafts-folder  . "/4.kebairia@gmail.com/[Gmail].Drafts")
                  (mu4e-sent-folder  . "/4.kebairia@gmail.com/[Gmail].Sent")
                  (mu4e-refile-folder  . "/4.kebairia@gmail.com/[Gmail].Archive")
                  (mu4e-trash-folder  . "/4.kebairia@gmail.com/[Gmail].Trash")
                  )))))
;; Configure the function to use for sending mail
(setq message-send-mail-function 'smtpmail-send-it)

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (visual-line-mode)
            (org-mu4e-compose-org-mode)
            (use-hard-newlines -1)
            (flyspell-mode)))

(add-hook 'mu4e-headers-mode-hook
          (defun my/mu4e-change-headers ()
            (interactive)
            (setq mu4e-headers-fields
                  `((:human-date . 10) ;; alternatively, use :date
                    (:flags . 6)
                    (:from . 22)
                    (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
                    (:size . 7)))))
(setq org-mu4e-convert-to-html t)
;;from the info manual
(setq mu4e-attachment-dir  "~/dwn")

(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)

(require 'org-mu4e)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)
;; don't ask when quitting
(setq mu4e-confirm-quit nil)
