;; Flycheck mode

(use-package flycheck
  :idle (global-flycheck-mode t)
  :config
  (progn
    (setq flycheck-idle-change-delay 2
          flycheck-check-syntax-automatically '(save new-line mode-enabled)
          flycheck-clang-include-path (quote ("/usr/include"))
          flycheck-keymap-prefix (kbd "C-c f"))

    ;; Enable better c++
    (add-hook 'c++-mode-hook (lambda()
                               (setq flycheck-clang-language-standard "c++11")))))

(provide 'setup-flycheck)
