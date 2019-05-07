
(req-package projectile
  :init (projectile-global-mode)
  :config (progn
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
            (define-key projectile-command-map "s" 'helm-projectile-rg)
            (define-key projectile-command-map "f" 'projectile-find-file-with-fallback))
  :bind (("<M-f1>" . projectile-ff-find-other-file)
         ("<f5>" . projectile-compile-with-fallback)))

(req-package helm-ag)

(req-package helm-projectile
  :init (helm-projectile-on)
  :config (setq helm-projectile-set-input-automatically nil))

;; Fallback to ido-find-file when not in a project (or over tramp)
(defun projectile-find-file-with-fallback ()
  (interactive)
  (if (or (not buffer-file-name) (file-remote-p buffer-file-name))
      (ido-find-file)
    (condition-case nil
        (projectile-find-file)
      (error (ido-find-file)))))

(defun projectile-ff-find-other-file ()
  (interactive)
  (let ((other-buffer (my/find-other-buffer)))
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
