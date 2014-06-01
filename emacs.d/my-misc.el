;; Miscellaneous settigns

(setq initial-scratch-message ";; This buffer is for notes you don't want to save, and for Lisp evaluation.

")

;; truncate long lines rather than wrapping
(set-default 'truncate-lines t)

;; Garbage collect less frequently
(setq gc-cons-threshold 1500000)

;; Move deleted files to trash
(setq delete-by-moving-to-trash t)

;; Always follow symbolic links to version controlled files
(setq vc-follow-symlinks t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; disaster for disassembly
(require 'disaster)
(add-hook 'c-mode-common-hook
          (lambda ()
	    (define-key c-mode-base-map (kbd "C-c d") 'disaster)))
(setq disaster-cxxflags "-std=c++11")

;; Fix D compile regex
(require 'compile)
(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
   1 2 nil (3 . 4)))

;; Add expand region
(require 'expand-region)

;; completion for M-x
(require 'smex)
(smex-initialize)

;; Move around with shift+arrow
(windmove-default-keybindings)

(require 'undo-tree)
(global-undo-tree-mode)

;; Set browse-kill-ring defaults
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(require 'project-explorer)
(setq pe/width 30)

;; Simple generic browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (getenv "BROWSER"))

;; Electric pair mode
(electric-pair-mode)

;; Subword mode for subword-backwords
(global-subword-mode)

;;Put backups/autosave in temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'yank-auto-indent)

;; Save cursor position between sessions
(require 'saveplace)
(setq-default save-place t)

(setq mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
(setq mouse-yank-at-point t)

(setq save-interprogram-paste-before-kill t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq scroll-conservatively 1000)

(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq apropos-do-all t)

(provide 'my-misc)
