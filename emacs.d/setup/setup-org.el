;; Org mode setup
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
(add-hook 'org-mode-hook 'visual-line-mode 'append)
(setq org-latex-create-formula-image-program 'imagemagick
      org-startup-folded 'showall
      org-src-fontify-natively t
      org-startup-with-inline-images 'inlineimages
      org-latex-to-pdf-process (list "latexmk -pdf %f")
      org-startup-indented (quote indent))

(provide 'setup-org)
