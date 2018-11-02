(require 'server)
(unless (server-running-p)
  (server-start))

;; remove bars early
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/setup/")
(add-to-list 'load-path "~/.emacs.d/lisp/idle-highlight/")
(add-to-list 'exec-path "/usr/local/bin")

(require 'setup-package)
(require-package 'req-package)

;; Simple y/n
(fset 'yes-or-no-p 'y-or-n-p)

(require 'req-package)
(setq use-package-always-ensure t)

;; Automatically recompile elisp buffers
(req-package auto-compile
  :config
  (progn (auto-compile-on-load-mode 1)
         (auto-compile-on-save-mode 1)))

(req-package exec-path-from-shell
  :config
  (progn (exec-path-from-shell-initialize)))

(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'slime '(require 'setup-slime))
(require 'misc)
(require 'setup-theme)
(require 'setup-flycheck)
(require 'setup-projectile)
(require 'setup-autocomplete)
(require 'setup-ido)
(require 'setup-recentf)
(require 'setup-hooks)
(require 'setup-helm)
(require 'setup-paredit)
(require 'setup-tramp)
(require 'my-misc)
(require 'functions)

(require 'setup-key-bindings)

(req-package-finish)

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (avy flycheck-rust exec-path-from-shell magit persistent-scratch which-key rust-mode helm-ag projectile-ripgrep helm-rg smart-mode-line solarized-theme auto-complete ido-completing-read+ solarized-dark-theme key-chord req-package el-get))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:background nil :foreground "#FF6E64" :underline t :weight bold))))
 '(flycheck-warning ((t (:background nil :foreground "#DEB542" :underline t :weight bold)))))
