(add-to-list 'load-path "~/.emacs.d/")
(setq delete-by-moving-to-trash t)

(require 'better-defaults)

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
          
;;spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;(require 'fill-column-indicator)

;; copy/past/undo hotkeys
(cua-mode t)

;; remove toolbar
(tool-bar-mode -1)

;; show column and line number
(column-number-mode 1)
(global-linum-mode 1)

;; Highlight current line
(global-hl-line-mode)
(set-face-background hl-line-face "gainsboro")


;;(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;;  (global-fci-mode 1)

;; trunkate long lines rather than wrapping
(set-default 'truncate-lines t)

;; re-indent on newline
(define-key global-map (kbd "RET") 'newline-and-indent)

;; pakages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; auto-complete setup
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

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

(global-set-key [C-backspace] 'whitespace-backward-delete-word)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fci-rule-column 80)
 '(inhibit-startup-screen t)
 '(org-startup-indented t)
 '(x-alt-keysym meta t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "tan" :foreground "gtk_selection_fg_color")))))
