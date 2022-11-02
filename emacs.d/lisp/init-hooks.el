

;; Make compile buffer show lines
(add-hook 'compilation-mode-hook (lambda () (setq truncate-lines nil)))

(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))

(add-hook 'term-mode-hook (lambda()
                            (setq global-hl-line-mode nil)))

;; Remove whitespace
(add-hook 'before-save-hook 'my/before-save-function)

;; Better defaults for Markdown mode
(add-hook 'markdown-mode-hook (lambda()
                                (visual-line-mode t)
                                (setq truncate-lines nil)
                                (setq word-wrap t)))

;; Spell checking in text-based modes
(use-package flyspell
  :init

  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)

  ; We use C-; for avy, so rebind flyspell
  (setq flyspell-auto-correct-binding (read-kbd-macro "M-c"))

  :config
  ; C-. is used for embark
  (define-key flyspell-mode-map (read-kbd-macro "C-.") nil))


(add-hook 'prog-mode-hook
          (lambda ()
             (define-key (current-local-map) (read-kbd-macro "<M-backspace>") 'backward-kill-sexp)
             (define-key (current-local-map) (read-kbd-macro "RET") 'my/new-line-dwim)
             (font-lock-add-keywords
              nil `((,(rx word-start (group (or "FIXME" "TODO" "NOCOMMIT")) word-end)
                     1 '((:weight bold)) t)))))

; Create any missing directory when using find-file
(defun create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist. Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'create-non-existent-directory)

(provide 'init-hooks)
