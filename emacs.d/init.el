(require 'server)
(unless (server-running-p)
  (server-start))

;; remove bars early
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/emacs-powerline")

;; Setup packages
(require 'setup-package)
(require 'use-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(magit
     auto-complete
     browse-kill-ring
     company
     company-c-headers
     dash
     disaster
     expand-region
     flx
     flx-ido
     flycheck
     haskell-mode
     git-gutter-fringe
     ido-ubiquitous
     ido-vertical-mode
     key-chord
     magit
     multiple-cursors
     paredit
     perspective
     project-explorer
     projectile
     slime
     smex
     solarized-theme
     undo-tree
     web-mode
     yasnippet
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Simple y/n
(fset 'yes-or-no-p 'y-or-n-p)

(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'slime '(require 'setup-slime))
(require 'misc)
(require 'setup-theme)
(require 'setup-flycheck)
(require 'setup-projectile)
(require 'setup-autocomplete)
(require 'setup-ido)
(require 'setup-hooks)
(require 'my-misc)
(require 'functions)

(require 'setup-key-bindings)

(provide 'init)
