(setq
 site-run-file nil                         ; No site-wide run-time initializations. 
 inhibit-default-init t                    ; No site-wide default library
 gc-cons-threshold most-positive-fixnum    ; Very large threshold for garbage
                                           ; collector during init
 package-enable-at-startup nil)            ; We'll use straight.el

(setq native-comp-eln-load-path
      (list (expand-file-name "eln-cache" user-emacs-directory)))

;; Reset garbage collector limit after init process has ended (8Mo)
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))
