(require 'server)
(unless (server-running-p)
  (server-start))

;; remove bars early
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'exec-path "/usr/local/bin")

(require 'init-package)

;; Simple y/n
(fset 'yes-or-no-p 'y-or-n-p)

(setq use-package-always-ensure t)

;; Automatically recompile elisp buffers
(use-package auto-compile
  :config
  (progn (auto-compile-on-load-mode 1)
         (auto-compile-on-save-mode 1)))

(use-package exec-path-from-shell
  :config
  (progn (exec-path-from-shell-initialize)))

(require 'misc)
(require 'init-theme)
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-autocomplete)
(require 'init-recentf)
(require 'init-hooks)
(require 'init-paredit)
(require 'my-misc)
(require 'functions)
(require 'init-minibuffer)

(require 'init-key-bindings)

(provide 'init)
