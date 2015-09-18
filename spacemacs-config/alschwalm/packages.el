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
      hy-mode
      paredit
      ))

;; List of packages to exclude.
(setq alschwalm-excluded-packages '())

(defun alschwalm/init-key-chord ()
  (use-package key-chord
    :config (progn
              ;; Key chords
              (key-chord-mode t)
              (key-chord-define-global "uu" 'undo-tree-visualize)
              (key-chord-define-global "JJ" 'switch-to-prev-buffer)
              (key-chord-define-global "KK" 'switch-to-next-buffer)
              ;; (key-chord-define-global ",," 'mc/mark-all-like-this-dwim)
              ;; (key-chord-define-global "vv" 'mc/mark-next-like-this-expand)
              ;; (key-chord-define-global "??" 'er/expand-region)
              )
    ))

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
      (add-hook 'scheme-mode-hook           #'enable-paredit-mode))
    ))

;; For each package, define a function alschwalm/init-<package-name>
;;
;; (defun alschwalm/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
