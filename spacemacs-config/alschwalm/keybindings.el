;; Additional global hotkeys
(global-set-key (read-kbd-macro "M-n") 'scroll-up)
(global-set-key (read-kbd-macro "M-p") 'scroll-down)
(global-set-key (read-kbd-macro "<C-tab>") 'other-window)
(global-set-key (read-kbd-macro "<C-backspace>") 'my/backward-delete-word)
(global-set-key (read-kbd-macro "C-x C-f") '
                my/helm-projectile-find-file-with-fallback)
(global-set-key (read-kbd-macro "C-'") 'er/expand-region)
(global-set-key (read-kbd-macro "C-x d") 'my/duplicate-buffer)
(global-set-key (read-kbd-macro "M-y") 'helm-show-kill-ring)
(global-set-key (read-kbd-macro "M-/") 'company-complete)

;; Tree undo
(global-unset-key (read-kbd-macro "C-z"))
(global-set-key (read-kbd-macro "C-z") 'undo-tree-undo)
(global-set-key (read-kbd-macro "C-S-z") 'undo-tree-redo)
