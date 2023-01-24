;; Additional global hotkeys
(global-set-key (read-kbd-macro "M-n") 'scroll-up)
(global-set-key (read-kbd-macro "M-p") 'scroll-down)
(global-set-key (read-kbd-macro "<C-tab>") 'other-window)
(global-set-key (read-kbd-macro "<C-backspace>") 'my/backward-delete-word)
(global-set-key (read-kbd-macro "C-a") 'my/back-to-indentation-or-beginning)
(global-set-key (read-kbd-macro "M-/") 'hippie-expand)
(global-set-key (read-kbd-macro "C-x C-b") 'ibuffer-other-window)
(global-set-key (read-kbd-macro "C-x 5") 'toggle-frame-split)
(global-set-key (read-kbd-macro "C-x 4") 'my/transpose-windows)
(global-set-key (read-kbd-macro "M-}") 'my/expand-window-split)
(global-set-key (read-kbd-macro "M-{") 'my/shrink-window-split)
(global-set-key (read-kbd-macro "C-x d") 'my/duplicate-buffer)
(global-set-key (read-kbd-macro "C-k") 'my/kill-line)
(global-set-key (read-kbd-macro "C-c p s") 'consult-ripgrep)
(global-set-key (read-kbd-macro "C-c p f") 'my/project-find-file)
(global-set-key (read-kbd-macro "C-x SPC") 'my/consult-combined-mark-ring)
(global-set-key (read-kbd-macro "M-,") 'my/pop-combined-mark-ring)

;; Key chords
(use-package key-chord
  :init (key-chord-mode t)
  :config
  (key-chord-define-global "uu" 'undo-tree-visualize)
  (key-chord-define-global "JJ" 'switch-to-prev-buffer)
  (key-chord-define-global "KK" 'switch-to-next-buffer))

;; Tree undo
(global-unset-key (read-kbd-macro "C-z"))
(global-set-key (read-kbd-macro "C-z") 'undo-tree-undo)
(global-set-key (read-kbd-macro "C-S-z") 'undo-tree-redo)

;; Add special escape sequences for terminal mode
(define-key function-key-map "\033[55;5~" [(control ?')])
(define-key function-key-map "\033[56;5~" [(control backspace)])
(define-key function-key-map "\033[57;5~" [(control ?\;)])
(define-key function-key-map "\033[80;5~" [(control ?\.)])
(define-key function-key-map "\033[81;5~" [(control ?\,)])


;; Modifier for mac
(setq mac-command-modifier 'control)

(provide 'init-key-bindings)
