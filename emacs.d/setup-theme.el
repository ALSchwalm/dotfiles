;; Load theme
(require 'solarized-dark-theme)

(require 'powerline)
(set-face-foreground 'minibuffer-prompt "cyan")
(setq solarized-high-contrast-mode-line t)
(setq custom-enabled-themes (quote (solarized-dark)))

;; Also setup cursor
(set-default 'cursor-type 'bar)
(blink-cursor-mode)
(set-default 'cursor-in-non-selected-windows nil)

;; show column and line number
(column-number-mode 1)

;; Git gutter fringe mode
(require 'git-gutter-fringe)
(global-git-gutter-mode t)

;; Better duplicate buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; always turn on, where available
(global-font-lock-mode t)

;; Highlight current line
(global-hl-line-mode)

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
