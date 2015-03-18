;; Save a list of recent files visited. (open recent file with C-x f)
(req-package recentf
  :init (progn (recentf-mode t)
               (add-to-list 'recentf-exclude "\\TAGS\\'")
               (add-to-list 'recentf-exclude "\\archive-contents\\'"))
  :bind (("C-x f" . recentf-ido-find-file))
  ;; just 20 is too recent
  :config (setq recentf-max-saved-items 100))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(provide 'setup-recentf)
