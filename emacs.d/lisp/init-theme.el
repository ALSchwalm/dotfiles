;; Load theme

(use-package smart-mode-line
  :init

  (setq sml/no-confirm-load-theme t)
  (setq sml/theme nil)
  (setq sml/name-width 40)
  (setf rm-blacklist "") ; rich-minority disable all minor modes
  (sml/setup))

(use-package solarized-theme
  :init

  (load-theme 'solarized-dark t)

  :config

  (add-hook 'window-configuration-change-hook
            (lambda ()
              (let ((line (face-attribute 'mode-line :underline)))
                (set-face-attribute 'mode-line          nil :overline   line)
                (set-face-attribute 'mode-line-inactive nil :overline   line)
                (set-face-attribute 'mode-line-inactive nil :underline  line)
                (set-face-attribute 'mode-line          nil :box        nil)
                (set-face-attribute 'mode-line-inactive nil :box        nil))))

  ;; Set the flycheck faces after make frame, because otherwise
  ;; things don't work in client/server mode
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-face-attribute 'flycheck-warning nil :background nil)
              (set-face-attribute 'flycheck-error nil :background nil)
              (set-face-attribute 'flycheck-info nil :background nil))))

(defun my/disable-background-terminal ()
  (unless (window-system (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'my/disable-background-terminal)

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
  (global-git-gutter-mode t))

;; always turn on, where available
(global-font-lock-mode t)

;; Highlight current line
(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)

;; linux style indents
(defconst my/custom-cpp-style
  '("linux"
    (c-offsets-alist . ((innamespace . [0])))
    (c-basic-offset . 4)))
(c-add-style "custom-cpp-style" my/custom-cpp-style)
(setq c-default-style "custom-cpp-style")

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Enable matching parens / braces
(show-paren-mode 1)

(use-package rust-mode)

(provide 'init-theme)
