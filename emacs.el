(add-to-list 'load-path "~/.emacs.d/")

;; pakages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  )

(setq delete-by-moving-to-trash t)

;; Load theme
(require 'solarized-dark-theme)

;; auto-complete setup
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
(add-to-list 'ac-modes 'rust-mode)

;; Required for forward-to-word and others
(require 'misc)

;; Add expand region
(require 'expand-region)

;; Git gutter fringe mode
(require 'git-gutter-fringe)
(global-git-gutter-mode t)
(setq git-gutter-fr:side 'right-fringe)
(fringe-helper-define 'git-gutter-fr:deleted nil "")

;; Make compile buffer show lines
(defun truncate-hook()
  (setq truncate-lines nil))
(add-hook 'compilation-mode-hook 'truncate-hook)

;; Use projectile everywhere
(projectile-global-mode)

;; Subword mode for subword-backwords
(global-subword-mode)

;; Fallback to ido-find-file when not in a project
(defun projectile-find-file-with-fallback ()
  (interactive)
  (condition-case nil
      (projectile-find-file)
    (error (ido-find-file))))

(defun ido-define-keys()
  (define-key ido-completion-map (kbd "C-f") 'ido-enter-find-file))
(add-hook 'ido-setup-hook 'ido-define-keys)

(defun projectile-ff-find-other-file-with-fallback()
  (interactive)
  (condition-case nil
      ((setq ff-always-try-to-create nil)
       (setq dirs (mapcar '(lambda (file)
                             (expand-file-name file (projectile-project-root)))
                          (projectile-current-project-dirs)))
       (setq ff-search-directories dirs)
       (ff-find-other-file))
    (error (ff-find-other-file))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

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

;; ido/flx/ido-ubiquitous
(require 'flx-ido)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)
(ido-mode t)
(flx-ido-mode t)
(setq ido-use-faces nil)

;; Better duplicate buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; copy/past/undo hotkeys
(cua-mode t)

;; Add matching parens / braces
(show-paren-mode 1)

;; remove bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; show column and line number
(column-number-mode 1)
(global-linum-mode 1)

;; trunkate long lines rather than wrapping
(set-default 'truncate-lines t)

(add-to-list 'load-path "~/.emacs.d//helm")
(require 'helm-config)

(defun ansi-term-default ()
  (interactive)
  (ansi-term "zsh" "localhost"))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Add basic delete word method
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (subword-backward arg) (point))))

;; Stop delete word at newlines
(defun whitespace-backward-delete-word ()
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (if (string-match "^\\s-+$" (buffer-substring (point-at-bol) (point)))
        (delete-region (point-at-bol) (point))
      (backward-delete-word 1))))

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun mc/mark-next-like-this-expand ()
  (interactive)
  (if (not (region-active-p))
      (er/expand-region 1))
  (mc/mark-next-like-this 1))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Simple y/n
(fset 'yes-or-no-p 'y-or-n-p) 

;; always turn on, where available
(global-font-lock-mode t)

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
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "C-c C-t") 'ansi-term-default)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key [M-f1] 'projectile-ff-find-other-file-with-fallback)
(global-set-key [f5] 'compile)
(global-set-key (kbd "C-;") 'ace-jump-word-mode)
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)
(global-set-key (kbd "M-s M-s") 'sudo-edit)

 ;; Key chords
(key-chord-mode t)
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "JJ" 'switch-to-prev-buffer)
(key-chord-define-global "vv" 'mc/mark-next-like-this-expand)
(key-chord-define-global ",," 'mc/mark-all-like-this-dwim)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; Make tab completions cycle
 '(completion-cycle-threshold t)
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))

 ;; linux style indents
 '(c-default-style "linux")
 '(c-basic-offset 4)

 '(x-select-enable-clipboard t)
 '(x-select-enable-primary t)
 '(save-interprogram-paste-before-kill t)
 '(apropos-do-all t)
 '(mouse-yank-at-point t)
 '(save-place-file (concat user-emacs-directory "places"))
 '(backup-directory-alist `(("." . ,(concat user-emacs-directory
                                            "backups"))))

 ;; Hide startup and shorter scratch
 '(inhibit-startup-screen t)
 '(initial-scratch-message ";; This buffer is for notes you don't want to save, and for Lisp evaluation.

 ")

 ;; Stop prompt from going into minibuffer
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(max-mini-window-height 2)

 ;; fix scrolling with mouse / hotkeys
 '(scroll-conservatively 1000)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
 '(org-startup-indented t)
 '(delete-by-moving-to-trash t)
 
 ;; Silence the bell
 '(ring-bell-function 'ignore)
 
 '(scroll-conservatively 1000))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(region ((t (:background "tan" :foreground "gtk_selection_fg_color"))))
 )
