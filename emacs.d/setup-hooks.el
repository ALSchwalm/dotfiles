

;; Make compile buffer show lines
(defun truncate-hook()
  (setq truncate-lines nil))
(add-hook 'compilation-mode-hook 'truncate-hook)

;; Fix haskell mode indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))

(defun ansi-term-default ()
  (interactive)
  (ansi-term "zsh" "localhost"))

(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))

;; Remove whitespace
(add-hook 'before-save-hook 'delete-whitespace-and-indent)

;; Better defaults for Markdown mode
(add-hook 'markdown-mode-hook (lambda()
                                (visual-line-mode t)
                                (setq truncate-lines nil)
                                (setq word-wrap t)))

(provide 'setup-hooks)
