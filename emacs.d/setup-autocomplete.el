(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)
(setq company-global-modes '(c-mode c++-mode lisp-mode emacs-lisp-mode))
(setq company-c-headers-path-system '("/usr/include/c++/4.9.0/"))
(setq company-clang-arguments '("-std=c++11"))
(setq company-dabbrev-code-time-limit 0)
(setq company-idle-delay 0)

;; auto-complete setup
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
(add-to-list 'ac-modes 'rust-mode)
(add-to-list 'ac-modes 'd-mode)
(setq ac-modes (-difference ac-modes company-global-modes))

(require 'yasnippet)
(yas/global-mode t)

(provide 'setup-autocomplete)
