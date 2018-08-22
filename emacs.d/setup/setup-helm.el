
(req-package helm
  :config
  (setq helm-quick-update t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-scroll-amount 8
        helm-candidate-number-limit 20
        helm-ff-file-name-history-use-recentf t)
  :bind (("C-c h" . helm-command-prefix)))

(provide 'setup-helm)
