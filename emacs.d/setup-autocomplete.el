
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
(add-to-list 'ac-modes 'rust-mode)
(add-to-list 'ac-modes 'd-mode)

(require 'yasnippet)
(yas-global-mode 1)

(use-package xcscope
  :init (cscope-setup)
  :config
  (progn
    (setq cscope-max-cscope-buffer-size 1)
    (setq cscope-display-times nil)))

(provide 'setup-autocomplete)
