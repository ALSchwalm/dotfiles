;; ido/flx/ido-ubiquitous
(require 'ido)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-use-filename-at-point nil
      ido-max-prospects 10)
(ido-mode t)

(require 'flx-ido)
(flx-ido-mode t)
(setq ido-use-faces nil)

(require 'ido-vertical-mode)
(ido-vertical-mode)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

(defun ido-define-keys()
  (define-key ido-completion-map (kbd "C-f") 'ido-enter-find-file))
(add-hook 'ido-setup-hook 'ido-define-keys)

(provide 'setup-ido)
