;;; packages.el --- alschwalm Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2015 Adam Schwalm
;;
;; Author: Adam Schwalm <adamschwalm@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq alschwalm-packages
    '(
      ;; package names go here
      key-chord
      elvish-mode
      hy-mode
      paredit
      parinfer))


;; List of packages to exclude.
(setq alschwalm-excluded-packages '())

(defun alschwalm/init-key-chord ()
  (use-package key-chord
    :config (progn
              ;; Key chords
              (key-chord-mode t)
              (key-chord-define-global "uu" 'undo-tree-visualize)
              (key-chord-define-global "JJ" 'switch-to-prev-buffer)
              (key-chord-define-global "KK" 'switch-to-next-buffer))))
              ;; (key-chord-define-global ",," 'mc/mark-all-like-this-dwim)
              ;; (key-chord-define-global "vv" 'mc/mark-next-like-this-expand)

(defun alschwalm/init-elvish-mode ()
  (use-package elvish-mode
    :config (progn)
            (add-hook 'elvish-mode-hook #'highlight-numbers--turn-off)))

(defun alschwalm/init-hy-mode ()
  (use-package hy-mode))

(defun alschwalm/init-paredit ()
  (use-package paredit
    :config
    (progn
      (define-key paredit-mode-map (kbd "M-)") 'paredit-beginning-of-sexp)
      (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
      (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
      (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
      (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
      (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
      (add-hook 'scheme-mode-hook           #'enable-paredit-mode))))

(defun alschwalm/init-parinfer ()
  (use-package parinfer
    :ensure t
    :bind
    (("C-," . parinfer-toggle-mode))
    :init
    (progn
      (setq parinfer-extensions
            '(defaults ; should be included.
               pretty-parens ; different paren styles for different modes.
               paredit ; Introduce some paredit commands.
               smart-tab ; C-b & C-f jump positions and smart shift with tab & S-tab.
               smart-yank)) ; Yank behavior depend on mode.
      (add-hook 'clojure-mode-hook #'parinfer-mode)
      (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
      (add-hook 'common-lisp-mode-hook #'parinfer-mode)
      (add-hook 'scheme-mode-hook #'parinfer-mode)
      (add-hook 'lisp-mode-hook #'parinfer-mode))))

;; For each package, define a function alschwalm/init-<package-name>
;;
;; (defun alschwalm/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
