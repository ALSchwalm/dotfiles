
(use-package paredit
  :config

  (define-key paredit-mode-map (kbd "M-)") 'paredit-beginning-of-sexp)
  (define-key paredit-mode-map (kbd "C-<backspace>") 'paredit-backward-kill-word)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(provide 'init-paredit)
