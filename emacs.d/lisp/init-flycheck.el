;; Flycheck mode

(defvar-local my/flycheck-eldoc-prior-point nil)

(defun my/flycheck-eldoc-post-command ()
  (when (and (not (boundp 'my/flycheck-eldoc-stop-recursion))
             (not (eq my/flycheck-eldoc-prior-point (point))))
   (let ((my/flycheck-eldoc-stop-recursion t))
     (setq my/flycheck-eldoc-prior-point (point))
     (my/clear-flycheck-and-eldoc))))

(defun my/clear-flycheck-and-eldoc ()
  (eldoc-message)
  (lv-delete-window))

(defun my/flycheck-display-errors-function (errors)
  (lv-message "%s" (flycheck-help-echo-all-error-messages errors)))

(use-package flycheck
  :after browse-kill-ring
  :init (global-flycheck-mode t)
  :config
  (setq flycheck-check-syntax-automatically '(save new-line mode-enabled)
        flycheck-clang-include-path (quote ("/usr/include"))
        flycheck-keymap-prefix (kbd "C-c f"))

  (add-hook 'post-command-hook #'my/flycheck-eldoc-post-command)
  (add-hook 'echo-area-clear-hook #'my/clear-flycheck-and-eldoc)

  ; This is needed to avoid a strange interaction between lv and browse-kill-ring
  (advice-add 'browse-kill-ring :before #'my/clear-flycheck-and-eldoc)

  (setq flycheck-display-errors-function #'my/flycheck-display-errors-function
        flycheck-display-errors-delay 0.3
        eldoc-idle-delay 0.3)

  (add-hook 'rust-mode-hook (lambda ()
                              (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))))

(use-package flycheck-rust
  :config
  (progn
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))



(provide 'init-flycheck)
