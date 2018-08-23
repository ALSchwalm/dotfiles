
(defun my/enable-company-mode ()
  "Enables company-mode"
  (global-company-mode)
  ;; Make sure emacs does the right thing with completion command
  (define-key (current-local-map) [remap hippie-expand] 'company-complete))

(req-package company
  :config
  (progn
    (setq company-backends '(company-capf company-files
                                          (company-dabbrev-code company-gtags company-etags company-keywords)
                                          company-oddmuse company-dabbrev)
          company-idle-delay 0.2
          company-dabbrev-downcase nil
          company-dabbrev-ignore-case nil
          company-transformers '(company-sort-by-occurrence)
          company-dabbrev-other-buffers t
          company-minimum-prefix-length 3)
    (setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))
    (setq company-require-match 'never)
    (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
    (add-hook 'prog-mode-hook 'my/enable-company-mode)))

;; Enable completions in python
(setq jedi:complete-on-dot t
      jedi:tooltip-method nil)

(provide 'setup-autocomplete)
