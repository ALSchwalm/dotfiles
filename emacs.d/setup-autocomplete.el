;; auto-complete setup
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
(add-to-list 'ac-modes 'rust-mode)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'yasnippet)
(yas/global-mode t)

(provide 'setup-autocomplete)
