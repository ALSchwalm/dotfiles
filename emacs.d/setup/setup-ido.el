;; ido/flx/ido-ubiquitous
(use-package ido
  :init (ido-mode t)
  :config
  (progn
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-case-fold nil
          ido-auto-merge-work-directories-length -1
          ido-use-filename-at-point nil
          ido-use-faces nil
          ido-max-prospects 10))
  :bind (("C-x C-f" . ido-find-file)))

(use-package flx-ido
  :init (flx-ido-mode t))

(use-package ido-vertical-mode
  :init (ido-vertical-mode)
  :config (progn
            (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)))

(use-package ido-completing-read+
  :init (ido-ubiquitous-mode t))

(provide 'setup-ido)
