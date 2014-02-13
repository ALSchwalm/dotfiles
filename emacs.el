(require 'server)
(unless (server-running-p)
  (server-start))

;; remove bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/emacs-powerline")

;; pakages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  )

;; show column and line number
(column-number-mode 1)

;; trunkate long lines rather than wrapping
(set-default 'truncate-lines t)

;; Load theme
(require 'solarized-dark-theme)
(setq solarized-distinct-fringe-background t)
(require 'powerline)
(set-face-foreground 'minibuffer-prompt "cyan")
(blink-cursor-mode)

(setq gc-cons-threshold 1500000)
(setq delete-by-moving-to-trash t)

;; auto-complete setup
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
(add-to-list 'ac-modes 'rust-mode)

(require 'yasnippet)
(yas/global-mode t)

;; Required for forward-to-word and others
(require 'misc)

;; Add expand region
(require 'expand-region)

;; Git gutter fringe mode
(require 'git-gutter-fringe)
(global-git-gutter-mode t)

;; Make compile buffer show lines
(defun truncate-hook()
  (setq truncate-lines nil))
(add-hook 'compilation-mode-hook 'truncate-hook)

;; Fix haskell mode indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Flycheck mode
(global-flycheck-mode t)

;; Use projectile everywhere
(projectile-global-mode)

(require 'project-explorer)
(setq pe/width 30)

(defun project-explorer-toggle ()
  (interactive)
  (if (pe/get-current-project-explorer-buffer)
      (kill-buffer (pe/get-current-project-explorer-buffer))
    (project-explorer-open)))

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

(defun projectile-ff-find-other-file()
  (interactive)
  (setq ff-always-try-to-create nil)
  (setq dirs (mapcar '(lambda (file)
                        (expand-file-name file (projectile-project-root)))
                     (projectile-current-project-dirs)))
  (setq cc-search-directories dirs)
  (ff-find-other-file nil 't))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))

(defun projectile-compile-with-fallback ()
  (interactive)
  (condition-case nil
      (call-interactively 'projectile-compile-project)
    (error (call-interactively 'compile))))

(defun refresh-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer) 
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))

;; Shorthand for C-x C-x then C-l
(defun cua-exchange-point-and-mark-center ()
  (interactive)
  (cua-exchange-point-and-mark ())
  (recenter-top-bottom))

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
               
;; Org mode setup
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
(add-hook 'org-mode-hook 'visual-line-mode 'append)
(setq org-pretty-entities t
      org-latex-create-formula-image-program 'imagemagick
      org-startup-folded 'showall
      org-src-fontify-natively t
      org-startup-with-inline-images 'inlineimages)

;;Put backups/autosave in temp directory        
(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))

;; Fix D compile regex
(require 'compile)
(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
   1 2 nil (3 . 4)))

(require 'yank-auto-indent)

;; Set browse-kill-ring defaults
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Slime
(require 'slime-autoloads)

;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/sbin/sbcl")
(require 'slime)
(slime-setup '(slime-fancy))

;; ido/flx/ido-ubiquitous
(require 'ido)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-use-filename-at-point nil
      ido-max-prospects 10)
(ido-mode t)

(require 'flx-ido)
(flx-ido-mode t)
(setq ido-use-faces nil)

(require 'ido-vertical-mode)
(ido-vertical-mode)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

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

(defun ansi-term-default ()
  (interactive)
  (ansi-term "zsh" "localhost"))

(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


(defun backward-word-stop-eol (arg)
  (interactive "p")
  (let ((start (point)))
    (save-restriction
      (save-excursion
        (move-beginning-of-line 1)
        (narrow-to-region start (point)))
      (subword-backward arg))))

;; Add basic delete word method
(defun backward-delete-word (arg)
  (interactive "p")
  (if (eq (point) (line-beginning-position))
      (backward-delete-char 1)
    (delete-region (point) (progn (backward-word-stop-eol arg) (point)))))

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

(defun sm-find-tag ()
  (interactive)
  (setq tags-table-list nil)
  (setq tags-file-name (concat (projectile-project-root) "TAGS"))
  (find-tag (funcall (or find-tag-default-function
                         (get major-mode 'find-tag-default-function)
                         'find-tag-default))))

(defun projectile-generate-tags ()
  (interactive)
  (setq default (concat "*." (file-name-extension (buffer-file-name))))
  (setq pattern (read-string (concat "Generate tags: ")
                                      default))
  (shell-command (concat "find " (projectile-project-root) " -name \"" pattern
                   "\" -print | etags -")))

;; Function to toggle vertical split to horizontal / vice versa
(defun toggle-frame-split ()
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil)))


(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; Simple y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; always turn on, where available
(global-font-lock-mode t)

;; Highlight current line
(global-hl-line-mode)

;; Additional global hotkeys
(global-set-key [C-backspace] 'backward-delete-word)
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
(global-set-key [M-f1] 'projectile-ff-find-other-file)
(global-set-key [f5] 'projectile-compile-with-fallback)
(global-set-key (kbd "C-;") 'ace-jump-word-mode)
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)
(global-set-key (kbd "M-s M-s") 'sudo-edit)
(global-set-key (kbd "M-.") 'sm-find-tag)
(global-set-key (kbd "C-x 5") 'toggle-frame-split)
(global-set-key (kbd "C-x 4") 'transpose-windows)
(global-set-key (kbd "M-`") 'project-explorer-toggle)
(global-set-key (kbd "M-m") 'helm-mini)
(global-set-key (kbd "C-x C-x") 'cua-exchange-point-and-mark-center)

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
 '(apropos-do-all t)
 '(backup-directory-alist (\` (("." \, (concat user-emacs-directory "backups")))))
 '(c-basic-offset 4)
 '(c-default-style "linux")
 '(completion-cycle-threshold t)
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(delete-by-moving-to-trash t)
 '(flycheck-clang-language-standard "c++11")
 '(flycheck-clang-include-path '("/usr/include/"))
 '(initial-scratch-message ";; This buffer is for notes you don't want to save, and for Lisp evaluation.

")
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
 '(mouse-yank-at-point t)
 '(org-startup-indented (quote indent))
 '(ring-bell-function (quote ignore) t)
 '(save-interprogram-paste-before-kill t)
 '(save-place-file (concat user-emacs-directory "places"))
 '(scroll-conservatively 1000)
 '(x-select-enable-clipboard t)
 '(x-select-enable-primary t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
