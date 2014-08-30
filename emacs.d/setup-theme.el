;; Load theme

(require 'solarized-dark-theme)
(require 'powerline)
(setq powerline-arrow-shape 'arrow14) ;; best for small fonts
(set-face-foreground 'minibuffer-prompt "cyan")
(setq solarized-high-contrast-mode-line t)
(setq custom-enabled-themes (quote (solarized-dark)))

(defun disable-background-terminal ()
  (unless (window-system (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'disable-background-terminal)

(setq default-frame-alist '((font . "Source Code Pro-11")
                            (vertical-scroll-bars . nil)))

;; Also setup cursor
(set-default 'cursor-type 'bar)
(blink-cursor-mode)
(set-default 'cursor-in-non-selected-windows nil)

;; show column and line number
(column-number-mode 1)

;; Git gutter mode
(require 'git-gutter)
(global-git-gutter-mode t)

;; Better duplicate buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; always turn on, where available
(global-font-lock-mode t)

;; Highlight current line
(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)

;; linux style indents
(setq c-default-style "linux"
      c-basic-offset 4)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Add matching parens / braces
(show-paren-mode 1)

(provide 'setup-theme)
