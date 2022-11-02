
(defun my/enable-company-mode ()
  "Enables company-mode"
  (global-company-mode)
  ;; Make sure emacs does the right thing with completion command
  (define-key (current-local-map) [remap hippie-expand] 'company-complete))

(use-package company
  :config
  (progn
    (setq company-backends '(company-capf company-files
                                          (company-dabbrev-code company-gtags company-etags company-keywords)
                                          company-oddmuse company-dabbrev)
          company-tooltip-width-grow-only t
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

    ;; Define for tab in terminal and gui
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map (kbd "<tab>") 'company-complete-selection)

    (add-hook 'prog-mode-hook 'my/enable-company-mode)))

;; Jumping for when we don't have gtags
(use-package dumb-jump
  :config
  (progn
    (setq dumb-jump-project-denoters '(".dumbjump" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".svn" "PkgInfo" "-pkg.el"))
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))

(use-package gxref
  :config
  (progn
    (add-to-list 'xref-backend-functions 'gxref-xref-backend)))

(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(setq xref-prompt-for-identifier nil)

(defun my/lsp-setup-rust ()
  (setq lsp-rust-analyzer-server-display-inlay-hints nil
        lsp-rust-analyzer-inlay-hints-mode nil
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t)
  (lsp))

(use-package lsp-mode
  :init
  (progn
    ;; Magic to prevent unrelated projects from being opened at once
    (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

    ;; We use flycheck-rust rather than LSP until there's some way to avoid
    ;; getting inconsistent results from rust-analyzer
    (setq lsp-diagnostics-disabled-modes '(rust-mode))

    (setq lsp-headerline-breadcrumb-enable nil
          lsp-keymap-prefix "C-c l"
          lsp-enable-file-watchers nil
          lsp-keep-workspace-alive nil
          lsp-lens-enable nil
          lsp-auto-guess-root t))
  :hook ((rust-mode . my/lsp-setup-rust)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-pyright
  :init
  (progn
    (setq lsp-pyright-multi-root nil))
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package consult-lsp)

(use-package yasnippet
  :init
  (progn
    (yas-global-mode 1)))

(provide 'init-autocomplete)
