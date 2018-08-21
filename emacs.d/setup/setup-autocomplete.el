
(defun my/enable-company-mode ()
  "Enables company-mode"
  (company-mode 1)
  ;; Make sure emacs does the right thing with completion command
  (define-key (current-local-map) [remap hippie-expand] 'company-complete))

(req-package company
  :config
  (progn
    (setq company-global-modes '(c++-mode lisp-mode emacs-lisp-mode)
          company-c-headers-path-system `( ,(nth 2 (directory-files "/usr/include/c++")))
          company-clang-arguments '("-std=c++1y")
          company-backends (delete 'company-semantic company-backends)
          company-dabbrev-code-time-limit 0
          company-idle-delay nil
          company-dabbrev-downcase nil
          company-dabbrev-ignore-case nil)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (add-hook 'prog-mode-hook 'my/enable-company-mode)))

;; semantic mode for parsing
(semantic-mode t)

(req-package cpputils-cmake)

;; auto-complete setup
(req-package auto-complete
  :config
  (progn
    (add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
    (ac-config-default)
    (setq ac-use-menu-map t)
    (define-key ac-menu-map (kbd "RET") 'ac-complete)
    (define-key ac-menu-map (kbd "<tab>") 'ac-next)
    (add-to-list 'ac-modes 'rust-mode)
    (add-to-list 'ac-modes 'd-mode)))

;; Enable completions in python
(setq jedi:complete-on-dot t
      jedi:tooltip-method nil)

;; Used in popups for autocomplete mode
(req-package pos-tip)

(req-package ggtags
  :config
  (progn
    (add-hook 'prog-mode-hook 'ggtags-mode)
    (define-key ggtags-mode-map (kbd "M-]") 'my/ggtags-find-reference)))

(defun my/ggtags-find-reference ()
  "Emit an error if gtags doesn't support finding references in the mode."
  (interactive)
  (if (not (member major-mode
                   '(c++-mode c-mode php-mode java-mode asm-mode)))
      (error "ggtags does not support finding references for this mode")
    (ggtags-find-reference (word-at-point))))

(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

(provide 'setup-autocomplete)
