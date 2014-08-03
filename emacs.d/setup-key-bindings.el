;; Additional global hotkeys
(global-set-key (read-kbd-macro "<C-backspace>") 'backward-delete-word)
(global-set-key (read-kbd-macro "<M-backspace>") 'backward-kill-sexp)
(global-set-key (read-kbd-macro "M-n") 'scroll-up)
(global-set-key (read-kbd-macro "M-p") 'scroll-down)
(global-set-key (read-kbd-macro "<C-tab>") 'other-window)
(global-set-key (read-kbd-macro "C-x C-f") 'projectile-find-file-with-fallback)
(global-set-key (read-kbd-macro "RET") 'new-line-dwim)
(global-set-key (read-kbd-macro "M-x") 'smex)
(global-set-key (read-kbd-macro "C-a") 'back-to-indentation-or-beginning)
(global-set-key (read-kbd-macro "M-/") 'hippie-expand)
(global-set-key (read-kbd-macro "C-x C-b") 'ibuffer-other-window)
(global-set-key (read-kbd-macro "C-c C-t") 'ansi-term-default)
(global-set-key (read-kbd-macro "C-s") 'isearch-forward-regexp)
(global-set-key (read-kbd-macro "C-r") 'isearch-backward-regexp)
(global-set-key (read-kbd-macro "C-M-s") 'isearch-forward)
(global-set-key (read-kbd-macro "C-M-r") 'isearch-backward)
(global-set-key (read-kbd-macro "<M-f1>") 'projectile-ff-find-other-file)
(global-set-key (read-kbd-macro "<f5>") 'projectile-compile-with-fallback)
(global-set-key (read-kbd-macro "C-;") 'ace-jump-word-mode)
(global-set-key (read-kbd-macro "C-'") 'er/expand-region)
(global-set-key (read-kbd-macro "C--") 'er/contract-region)
(global-set-key (read-kbd-macro "M-s M-s") 'sudo-edit)
(global-set-key (read-kbd-macro "M-.") 'sm-find-tag)
(global-set-key (read-kbd-macro "C-x 5") 'toggle-frame-split)
(global-set-key (read-kbd-macro "C-x 4") 'transpose-windows)
(global-set-key (read-kbd-macro "M-`") 'project-explorer-toggle)
(global-set-key (read-kbd-macro "M-]") 'expand-window-split)
(global-set-key (read-kbd-macro "M-[") 'shrink-window-split)
(global-set-key (read-kbd-macro "C-x C-x") 'exchange-point-and-mark-center)
(global-set-key (read-kbd-macro "C-x d") 'duplicate-buffer)
(global-set-key (read-kbd-macro "C-x f") 'recentf-ido-find-file)
(global-set-key (read-kbd-macro "C-\\") 'hs-toggle-hiding)

(require 'focus)
(global-set-key (read-kbd-macro "<f11>") 'focus-toggle-focus)

 ;; Key chords
(key-chord-mode t)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "JJ" 'switch-to-prev-buffer)
(key-chord-define-global "KK" 'switch-to-next-buffer)
(key-chord-define-global ",," 'mc/mark-all-like-this-dwim)
(key-chord-define-global "vv" 'mc/mark-next-like-this-expand)
(key-chord-define-global ";;" 'er/expand-region)

;; Tree undo
(global-unset-key (read-kbd-macro "C-z"))
(global-set-key (read-kbd-macro "C-z") 'undo-tree-undo)
(global-set-key (read-kbd-macro "C-S-z") 'undo-tree-redo)

(provide 'setup-key-bindings)
