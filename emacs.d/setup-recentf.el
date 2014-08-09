;; Save a list of recent files visited. (open recent file with C-x f)
(use-package recentf
  :init (recentf-mode 1)
  :bind (((read-kbd-macro "C-x f") . recentf-ido-find-file)))
  ;; just 20 is too recent
  :config (progn
            (setq recentf-max-saved-items 100)
            (add-to-list 'recentf-exclude "\\TAGS\\'")
            (add-to-list 'recentf-exclude "\\archive-contents\\'"))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(provide 'setup-recentf)
