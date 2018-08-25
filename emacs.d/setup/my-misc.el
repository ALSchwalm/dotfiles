;; Miscellaneous settigns

(setq initial-scratch-message ";; This buffer is for notes, and for Lisp evaluation.

")

(req-package persistent-scratch
  :init (persistent-scratch-setup-default))

;; disable the bell (especially important on mac)
(setq ring-bell-function 'ignore)

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
(req-package disaster
  :commands disaster
  :config
  (progn
    (add-hook 'c-mode-common-hook
              (lambda ()
                (define-key c-mode-base-map (kbd "C-c d") 'disaster)))
    (setq disaster-cxxflags "-std=c++11")))

;; Don't allow me to kill the scratch
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; Add expand region
(req-package expand-region)

;; Move around with shift+arrow
(windmove-default-keybindings)

(req-package undo-tree
  :init (global-undo-tree-mode)
  :config (progn
            (let ((undo-dir (concat user-emacs-directory "undo")))
             (unless (file-exists-p undo-dir)
               (make-directory undo-dir))
             (setq undo-tree-mode-lighter " UT"
                   undo-tree-auto-save-history t
                   undo-tree-enable-undo-in-region nil
                   undo-tree-history-directory-alist
                   `(("." . ,undo-dir))))))

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

(req-package flex-isearch
  :init (global-flex-isearch-mode)
  :config (setq flex-isearch-auto 'on-failed))

;; Save cursor position between sessions
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory "places"))

(delete-selection-mode 1)
(setq mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
(setq mouse-yank-at-point t)
(setq scroll-conservatively 1000)

(setq save-interprogram-paste-before-kill t)

(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq apropos-do-all t)

(req-package popwin
  :config
  (progn
    (popwin-mode)
    ;; kill ring
    (push "*Kill Ring*" popwin:special-display-config)
    (push '(" *undo-tree*" :position) popwin:special-display-config)
    (push '("*ggtags-global*" :stick t) popwin:special-display-config)
    (push '("^\\*helm.*\\*$" :regexp t :position bottom)
          popwin:special-display-config)
    (global-set-key (kbd "C-c w") popwin:keymap)))

;; Probably not looking at files vc'd under bazaar
;; (setq vc-handled-backends '(SVN Git))

(req-package which-key
  :config (which-key-mode))

(provide 'my-misc)
