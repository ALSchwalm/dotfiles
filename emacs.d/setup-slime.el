;; Slime
(require 'slime-autoloads)

;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/sbin/sbcl")
(require 'slime)
(slime-setup '(slime-fancy))

(provide 'setup-slime)
