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

(require 'setup-package)
(require-package 'req-package)

;; Simple y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Wrap use-package, but imply ':ensure'
(require 'req-package)

;; Automatically recompile elisp buffers
(req-package auto-compile
  :config
  (progn (auto-compile-on-load-mode 1)
         (auto-compile-on-save-mode 1)))

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
(require 'my-misc)
(require 'functions)

(require 'setup-key-bindings)

(req-package-finish)

(provide 'init)
