;; Slime
(req-package slime-autoloads)

;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/sbin/sbcl")
(req-package slime
  :config
  (progn (slime-setup '(slime-fancy))
         (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
         (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(provide 'setup-slime)
