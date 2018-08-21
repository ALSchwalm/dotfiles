
(req-package helm
  :config
  (setq helm-quick-update                     t ; do not display invisible candidates
        helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)

  :bind (("C-c h" . helm-command-prefix)))

(provide 'setup-helm)
