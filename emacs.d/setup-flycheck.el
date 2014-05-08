;; Flycheck mode
(global-flycheck-mode t)

;; Best C++
(add-hook 'c++-mode-hook (lambda() (setq flycheck-clang-language-standard "c++11")))
(setq flycheck-clang-include-path (quote ("/usr/include")))

(provide 'setup-flycheck)
