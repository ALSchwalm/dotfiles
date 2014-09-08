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

(use-package idle-highlight-mode
  :init (add-hook 'prog-mode-hook 'idle-highlight-mode))

;; disaster for disassembly
(use-package disaster
  :commands disaster
  :config
  (progn
    (add-hook 'c-mode-common-hook
              (lambda ()
                (define-key c-mode-base-map (kbd "C-c d") 'disaster)))
    (setq disaster-cxxflags "-std=c++11")))


;; Fix D compile regex
(use-package compile
  :config (add-to-list
           'compilation-error-regexp-alist
           '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
             1 2 nil (3 . 4))))

;; Don't allow me to kill the scratch
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; Add expand region
(use-package expand-region)

;; completion for M-x
(use-package smex
  :init (smex-initialize)
  :config (smex-auto-update nil))

;; Toggle the perspective mode
(persp-mode)

;; Move around with shift+arrow
(windmove-default-keybindings)

(use-package undo-tree
  :init (global-undo-tree-mode)
  :config
  (progn
    ;; Keep region while undoing in region
    (defadvice undo-tree-undo (around keep-region activate)
      (if (use-region-p)
          (let ((m (set-marker (make-marker) (mark)))
                (p (set-marker (make-marker) (point))))
            ad-do-it
            (goto-char p)
            (set-mark m)
            (set-marker p nil)
            (set-marker m nil))
        ad-do-it))))

;; Set browse-kill-ring defaults
(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

(use-package project-explorer
  :config (setq pe/width 30)
  :bind (((read-kbd-macro "M-`") . project-explorer-toggle)))

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

(use-package flex-isearch
  :init (global-flex-isearch-mode)
  :config (setq flex-isearch-auto 'on-failed)
  :bind (((read-kbd-macro "C-s") . flex-isearch-forward)
         ((read-kbd-macro "C-r") . flex-isearch-backward)))

;; Save cursor position between sessions
(use-package saveplace
  :config (setq-default save-place t))

(use-package focus
  :bind (((read-kbd-macro "<f11>") . focus-toggle-focus)))

(setq mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
(setq mouse-yank-at-point t)

(setq save-interprogram-paste-before-kill t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq scroll-conservatively 1000)

(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq apropos-do-all t)

(use-package popwin
  :init (popwin-mode)
  :config
  (progn
    ;; kill ring
    (push "*Kill Ring*" popwin:special-display-config)
    (push '("*ggtags-global*" :stick t) popwin:special-display-config)
    ))

(provide 'my-misc)
