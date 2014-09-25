
(defun my/enable-company-mode ()
  "Enables company-mode"
  (company-mode 1)
  ;; Make sure emacs does the right thing with completion command
  (define-key (current-local-map) [remap hippie-expand] 'company-complete))

(require 'company)
(setq company-global-modes '(c++-mode lisp-mode emacs-lisp-mode)
      company-c-headers-path-system '("/usr/include/c++/4.9.0/")
      company-clang-arguments '("-std=c++11")
      company-dabbrev-code-time-limit 0
      company-idle-delay nil
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil)
(add-hook 'prog-mode-hook 'my/enable-company-mode)

;; auto-complete setup
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
(setq ac-use-menu-map t)
(add-to-list 'ac-modes 'rust-mode)
(add-to-list 'ac-modes 'd-mode)

;; Used in popups for autocomplete mode
(use-package pos-tip)

(require 'yasnippet)
(yas-global-mode 1)

(use-package ggtags
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
