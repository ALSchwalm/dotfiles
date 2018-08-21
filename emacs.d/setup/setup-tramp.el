;; Configure tramp

;; Quiet down tramp
(setq tramp-verbose 2)

;; Speed up directory completions
(setq tramp-completion-reread-directory-timeout nil)

(setq tramp-default-method "ssh"
      tramp-default-user "adam")

;; Give tramp buffers appropriate names
(add-hook 'find-file-hook 'add-server-postfix)

(provide 'setup-tramp)
