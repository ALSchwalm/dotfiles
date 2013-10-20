(add-to-list 'load-path "~/.emacs.d/")

;; pakages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  )
  
(setq delete-by-moving-to-trash t)

(require 'better-defaults)

;; Use projectile everywhere
(projectile-global-mode)

;; Fallback to ido-find-file when not in a project
(defun projectile-find-file-with-fallback ()
  (interactive)
  (condition-case nil
      (projectile-find-file)
    (error (ido-find-file))))

;; completion for M-x
(smex-initialize)

 ;; Always follow symbolic links to version controlled files
(setq vc-follow-symlinks t)

(require 'undo-tree)
(global-undo-tree-mode)

;; Tree undo
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; linux style indents
(setq c-default-style "linux"
      c-basic-offset 4)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties (quote 
	(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
               
;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;;Put backups/autosave in temp directory        
(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))

;; Set browse-kill-ring defaults
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; copy/past/undo hotkeys
(cua-mode t)

;; remove bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

 ;; Line format: N <contents>
(setq linum-format "%3d")

;; show column and line number
(column-number-mode 1)
(global-linum-mode 1)

;; supress bell
(setq ring-bell-function 'ignore)

;; trunkate long lines rather than wrapping
(set-default 'truncate-lines t)

;; auto-complete setup
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

(add-to-list 'load-path "~/.emacs.d//helm")
(require 'helm-config)

;; Add basic delete word method
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

;; Stop delete word at newlines
(defun whitespace-backward-delete-word ()
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (if (string-match "^\\s-+$" (buffer-substring (point-at-bol) (point)))
        (delete-region (point-at-bol) (point))
      (backward-delete-word 1))))

;; Load theme
(require 'solarized-dark-theme)

;; Highlight current line
(global-hl-line-mode)

;; Additional global hotkeys
(global-set-key [C-backspace] 'whitespace-backward-delete-word)
(global-set-key (kbd "M-n") 'scroll-up)
(global-set-key (kbd "M-p") 'scroll-down)
(global-set-key [C-tab] 'other-window)
(global-set-key [?\C-x ?\C-f] 'projectile-find-file-with-fallback)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(inhibit-startup-screen t)
 '(completion-cycle-threshold t)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
 '(org-startup-indented t)
 '(max-mini-window-height 2)
 '(initial-scratch-message  ";; This buffer is for notes you don't want to save, and for Lisp evaluation.

 ")
 '(scroll-conservatively 1000))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(region ((t (:background "tan" :foreground "gtk_selection_fg_color"))))
 )
