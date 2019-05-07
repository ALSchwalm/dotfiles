;; Additional global hotkeys
(global-set-key (read-kbd-macro "M-n") 'scroll-up)
(global-set-key (read-kbd-macro "M-p") 'scroll-down)
(global-set-key (read-kbd-macro "<C-tab>") 'other-window)
(global-set-key (read-kbd-macro "<C-backspace>") 'my/backward-delete-word)
(global-set-key (read-kbd-macro "M-x") 'helm-M-x)
(global-set-key (read-kbd-macro "C-a") 'my/back-to-indentation-or-beginning)
(global-set-key (read-kbd-macro "M-/") 'hippie-expand)
(global-set-key (read-kbd-macro "C-x b") 'my/helm-buffers-list)
(global-set-key (read-kbd-macro "C-x C-b") 'ibuffer-other-window)
(global-set-key (read-kbd-macro "C-c C-t") 'ansi-term-default)
(global-set-key (read-kbd-macro "C-;") 'avy-goto-char-timer)
(global-set-key (read-kbd-macro "C-'") 'er/expand-region)
(global-set-key (read-kbd-macro "C--") 'er/contract-region)
(global-set-key (read-kbd-macro "M-s M-s") 'my/sudo-edit)
(global-set-key (read-kbd-macro "C-x 5") 'toggle-frame-split)
(global-set-key (read-kbd-macro "C-x 4") 'my/transpose-windows)
(global-set-key (read-kbd-macro "M-}") 'my/expand-window-split)
(global-set-key (read-kbd-macro "M-{") 'my/shrink-window-split)
(global-set-key (read-kbd-macro "C-x C-x") 'my/pop-global-mark)
(global-set-key (read-kbd-macro "C-x d") 'my/duplicate-buffer)
(global-set-key (read-kbd-macro "C-\\") 'hs-toggle-hiding)
(global-set-key (read-kbd-macro "C-k") 'my/kill-line)
(global-set-key (read-kbd-macro "M-y") 'helm-show-kill-ring)
(global-set-key (read-kbd-macro "M-.") 'my/jump-to-definition-dwim)

 ;; Key chords
(req-package key-chord
  :init (key-chord-mode t)
  :ensure t
  :config
  (progn
    (key-chord-define-global "uu" 'undo-tree-visualize)
    (key-chord-define-global "JJ" 'switch-to-prev-buffer)
    (key-chord-define-global "KK" 'switch-to-next-buffer)))

;; Tree undo
(global-unset-key (read-kbd-macro "C-z"))
(global-set-key (read-kbd-macro "C-z") 'undo-tree-undo)
(global-set-key (read-kbd-macro "C-S-z") 'undo-tree-redo)

;; Add special escape sequences for terminal mode
(define-key function-key-map "\033[55;5~" [(control ?')])
(define-key function-key-map "\033[56;5~" [(control backspace)])
(define-key function-key-map "\033[57;5~" [(control ?\;)])

;; Modifier for mac
(setq mac-command-modifier 'control)

(provide 'setup-key-bindings)
