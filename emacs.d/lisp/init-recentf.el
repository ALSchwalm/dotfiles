;; Save a list of recent files visited. (open recent file with C-x f)
(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never)
  (recentf-mode t)
  (add-to-list 'recentf-exclude "\\TAGS\\'")
  (add-to-list 'recentf-exclude "\\archive-contents\\'")

  ;; just 20 is too recent
  :config
  (setq recentf-max-saved-items 100))

(provide 'init-recentf)
