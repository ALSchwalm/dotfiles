;; Load theme

(require 'solarized-dark-theme)
(setq sml/no-confirm-load-theme t)
(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'respectful)
(setq sml/name-width 20)
(setq sml/mode-width 'right)
(set-face-foreground 'minibuffer-prompt "#268bd2")
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
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(defconst custom-cpp-style
  '("linux"
    (c-offsets-alist . ((innamespace . [0])))
    (c-basic-offset . 4)))
(c-add-style "custom-cpp-style" custom-cpp-style)
(setq c-default-style "custom-cpp-style")

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Add matching parens / braces
(show-paren-mode 1)

(provide 'setup-theme)
