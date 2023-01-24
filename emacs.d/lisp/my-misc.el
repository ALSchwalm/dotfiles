;; Miscellaneous settings

(setq initial-scratch-message ";; This buffer is for notes, and for Lisp evaluation.

")

(use-package persistent-scratch
  :init (persistent-scratch-setup-default)
  :config
  (setq persistent-scratch-backup-directory (concat user-emacs-directory "scratch"))
  (with-current-buffer "*scratch*"
    (persistent-scratch-mode)))

;; disable the bell (especially important on mac)
(setq ring-bell-function 'ignore)

;; truncate long lines rather than wrapping
(set-default 'truncate-lines t)

;; Garbage collect less frequently
(setq gc-cons-threshold 1500000)
(use-package gcmh
  :init (gcmh-mode 1))

;; Move deleted files to trash
(setq delete-by-moving-to-trash t)

;; Use aspell for spelling
(setq ispell-program-name "aspell")

;; Always follow symbolic links to version controlled files
(setq vc-follow-symlinks t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Don't allow me to kill the scratch
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; Add expand region
(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C--" . er/contract-region)))

;; Add avy-mode
(use-package avy
  :bind ("C-;" . avy-goto-char-timer))

;; Move around with shift+arrow
(windmove-default-keybindings)

(use-package undo-tree
  :init (global-undo-tree-mode)
  :config

  (defun my/undo-tree-save-history (undo-tree-save-history &rest args)
    (let ((message-log-max nil)
          (inhibit-message t))
      (apply undo-tree-save-history args)))

  ; Suppress messages about undo-tree saving
  (advice-add 'undo-tree-save-history :around 'my/undo-tree-save-history)

  (let ((undo-dir (concat user-emacs-directory "undo")))
    (unless (file-exists-p undo-dir)
      (make-directory undo-dir))
    (setq undo-tree-mode-lighter " UT"
          undo-tree-enable-undo-in-region nil
          undo-tree-history-directory-alist `(("." . ,undo-dir))
          )))

;; Simple generic browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (or (getenv "BROWSER") "firefox"))

;; Electric pair mode
(electric-pair-mode)

;; Subword mode for subword-backwords
(global-subword-mode)

;;Put backups/autosave in temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Save cursor position between sessions
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory "places"))

(delete-selection-mode 1)
(setq mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
(setq mouse-yank-at-point t)
(setq scroll-conservatively 1000)

(setq save-interprogram-paste-before-kill t)
(setq kill-do-not-save-duplicates t)

(setq select-enable-clipboard t)

(use-package popwin
  :config

  (popwin-mode)
  ;; kill ring
  (push "*Kill Ring*" popwin:special-display-config)
  (push '(" *undo-tree*" :position bottom) popwin:special-display-config)
  (push '("*ggtags-global*" :stick t) popwin:special-display-config)
  (global-set-key (kbd "C-c w") popwin:keymap))

;; Probably not looking at files vc'd under bazaar
(setq vc-handled-backends '(Git Hg))

(use-package magit)

(use-package sudo-edit)

(use-package which-key
  :config (which-key-mode))

(defun my/change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))
(add-hook 'window-configuration-change-hook 'my/change-window-divider)

(when (not (display-graphic-p))
  (require 'term/xterm)
  (setq xterm-extra-capabilities '(setSelection getSelection modifyOtherKeys))
  (setq xterm-set-window-title t)

  (add-hook 'tty-setup-hook
            (lambda ()
              (terminal-init-xterm)
              (xterm-mouse-mode))))

(provide 'my-misc)
