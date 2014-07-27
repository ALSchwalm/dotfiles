

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

(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)
                            (setq global-hl-line-mode nil)))

;; Remove whitespace
(add-hook 'before-save-hook 'delete-whitespace-and-indent)

;; Better defaults for Markdown mode
(add-hook 'markdown-mode-hook (lambda()
                                (visual-line-mode t)
                                (setq truncate-lines nil)
                                (setq word-wrap t)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Spell checking in comments
(add-hook 'c-mode-common-hook (lambda ()
                                (flyspell-prog-mode)))

;; Add search hotkey to c++-mode
(add-hook 'c++-mode-hook (lambda ()
                           (local-set-key (read-kbd-macro "<f1>") 'search-cpp-symbol-at-point)))

(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t)))

(eval-after-load "paredit"
'(progn
   (define-key paredit-mode-map (kbd "M-)") 'paredit-beginning-of-sexp)))

;; Auto-update smex
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)

(provide 'setup-hooks)
