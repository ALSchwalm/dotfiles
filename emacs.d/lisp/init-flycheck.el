;; Flycheck mode

(use-package flycheck
  :init (global-flycheck-mode t)
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save new-line mode-enabled)
          flycheck-clang-include-path (quote ("/usr/include"))
          flycheck-keymap-prefix (kbd "C-c f"))

    (add-hook 'rust-mode-hook (lambda ()
                                (setq-local flycheck-check-syntax-automatically '(save mode-enabled))))))

(use-package flycheck-rust
  :config
  (progn
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

(provide 'init-flycheck)
