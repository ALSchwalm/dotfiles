
(defun my/enable-company-mode ()
  "Enables company-mode"
  (global-company-mode)
  ;; Make sure emacs does the right thing with completion command
  (define-key (current-local-map) [remap hippie-expand] 'company-complete))

(use-package company
  :config
  (setq company-backends '(company-capf company-files
                                        (company-dabbrev-code company-gtags company-etags company-keywords)
                                        company-dabbrev)
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

  (add-hook 'prog-mode-hook 'my/enable-company-mode))

(use-package dumb-jump
  :config

  ;; A trick to avoid searching the home directory
  (setq dumb-jump-default-project "")

  ;; Remove Makefile from the list, as it kind of breaks recursive make
  (setq dumb-jump-project-denoters '(".dumbjump" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".svn" "PkgInfo" "-pkg.el"))
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(setq xref-prompt-for-identifier nil)

(defun my/lsp-setup-rust ()
  (setq lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t)
  (lsp))

(use-package lsp-mode
  :init
  ;; Magic to prevent unrelated projects from being opened at once
  (advice-add 'lsp :before (lambda (&rest _args)
                             (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

  (setq lsp-headerline-breadcrumb-enable nil
        lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        lsp-keep-workspace-alive nil
        lsp-lens-enable nil
        lsp-auto-guess-root t
        lsp-signature-doc-lines 5)

  :config
  (set-face-attribute 'lsp-face-highlight-read nil :inherit nil)
  (set-face-attribute 'lsp-face-highlight-write nil :inherit nil)

  :hook ((rust-mode . my/lsp-setup-rust)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-pyright
  :init
  (setq lsp-pyright-multi-root nil)

  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package consult-lsp
  :config

  ;; Define a new annotation build that ensures no newlines
  ;; in the diagnostic messages (because vertico doesn't deal
  ;; with multi-line candidates)
  (defun my/consult-lsp--diagnostics-annotate-builder ()
    (let* ((width (length (number-to-string (line-number-at-pos
                                             (point-max)
                                             consult-line-numbers-widen)))))
      (lambda (cand)
        (let* ((diag (cdr (get-text-property 0 'consult--candidate cand))))
          (list cand
                (format "%-5s " (consult-lsp--diagnostics--severity-to-level diag))
                (concat
                 (format "%s" (subst-char-in-string ?\n ?\s
                                                    (lsp:diagnostic-message diag)))
                 (when-let ((source (consult-lsp--diagnostics--source diag)))
                   (propertize (format " - %s" source) 'face 'font-lock-doc-face))))))))

  (setq consult-lsp-diagnostics-annotate-builder-function
        'my/consult-lsp--diagnostics-annotate-builder))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(provide 'init-autocomplete)
