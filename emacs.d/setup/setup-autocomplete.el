
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

(defun my/lsp-setup-rust ()
  (cl-defgeneric lsp-clients-extract-signature-on-hover (contents _server-id)
    "Extract module and function information from rust-analyzer for LSP"
    (if (not (eq major-mode 'rust-mode))
        (car (s-lines (s-trim (lsp--render-element contents))))
      (let* ((type-info (s-trim (car (s-split "---" (lsp--render-element contents) t))))
             (parts (mapcar (-compose (lambda (part) (s-chop-suffix "," part)) 's-trim)
                            (s-split "\n" type-info t))))
        (if (eq (length parts) 1)
            (car parts)
          (format "[%s] %s" (car parts) (s-join " " (cdr parts)))))))

  (setq lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-inlay-hints-mode t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t)

  (lsp))

(use-package lsp-mode
  :init
  (progn
    ;; Magic to prevent unrelated projects from being opened at once
    (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

    (setq lsp-keymap-prefix "C-c l")
    (setq lsp-headerline-breadcrumb-enable nil
          lsp-keep-workspace-alive nil
          lsp-auto-guess-root t))
  :hook ((rust-mode . my/lsp-setup-rust)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-pyright
  :ensure t
  :init
  (progn
    (setq lsp-pyright-multi-root nil))
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))


(provide 'setup-autocomplete)
