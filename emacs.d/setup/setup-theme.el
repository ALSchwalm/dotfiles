;; Load theme

(use-package solarized-theme)

(use-package smart-mode-line
  :config
  (progn (setq sml/no-confirm-load-theme t)
         (sml/setup)
         (sml/apply-theme 'respectful)
         (setq sml/name-width 20)
         (setq sml/mode-width 'right)))

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
(use-package git-gutter
  :config
  (progn (global-git-gutter-mode t)))

;; always turn on, where available
(global-font-lock-mode t)

;; Highlight current line
(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)

;; linux style indents
(defconst custom-cpp-style
  '("linux"
    (c-offsets-alist . ((innamespace . [0])))
    (c-basic-offset . 4)))
(c-add-style "custom-cpp-style" custom-cpp-style)
(setq c-default-style "custom-cpp-style")

(use-package web-mode
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Add matching parens / braces
(show-paren-mode 1)
(use-package mic-paren
  :config (progn
            (paren-activate)
            (setq show-paren-delay 0)))

(use-package rust-mode)

(provide 'setup-theme)
