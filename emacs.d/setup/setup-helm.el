
(req-package helm
  :config
  (progn
    (setq helm-quick-update t
          helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-scroll-amount 8
          helm-buffer-skip-remote-checking t
          helm-candidate-number-limit 20
          helm-ff-file-name-history-use-recentf t)
    ;; Avoid fuzzy searches with helm-do-ag
    (defun helm-ag--join-patterns (input)
      input))
  :bind (("C-c h" . helm-command-prefix)))

(req-package helm-gtags)

(provide 'setup-helm)
