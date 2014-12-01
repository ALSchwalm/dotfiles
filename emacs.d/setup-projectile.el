
(req-package projectile
  :init (projectile-global-mode)
  :bind (((read-kbd-macro "C-x C-f") . projectile-find-file-with-fallback)
         ((read-kbd-macro "<M-f1>") . projectile-ff-find-other-file)
         ((read-kbd-macro "<f5>") . projectile-compile-with-fallback)))

(req-package helm-projectile
  :init (helm-projectile-on)
  :config (define-key projectile-command-map "s" 'helm-projectile-ag))

;; Fallback to ido-find-file when not in a project
(defun projectile-find-file-with-fallback ()
  (interactive)
  (condition-case nil
      (projectile-find-file)
    (error (ido-find-file))))

(defun projectile-ff-find-other-file ()
  (interactive)
  (let ((other-buffer (find-other-buffer)))
    (if (not other-buffer)
        (condition-case nil
            (let ((cc-search-directories (mapcar (lambda (file)
                                                   (expand-file-name file (projectile-project-root)))
                                                 (projectile-current-project-dirs))))
              (ff-find-other-file nil t))
          (error (ff-find-other-file)))
      (switch-to-buffer other-buffer))))

(defun projectile-compile-with-fallback ()
  (interactive)
  (condition-case nil
      (call-interactively 'projectile-compile-project)
    (error (call-interactively 'compile))))

(defun projectile-generate-tags ()
  (interactive)
  (setq default (concat "*." (file-name-extension (buffer-file-name))))
  (setq pattern (read-string (concat "Generate tags: ")
                             default))
  (let ((default-directory (projectile-project-root)))
    (shell-command (format "find %s -type f -name \"%s\" | etags -" default-directory pattern))))


(provide 'setup-projectile)
