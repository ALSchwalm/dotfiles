(defun my/embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(defun my/embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(use-package embark-consult
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  :demand t

  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  (set-face-attribute 'embark-target nil :inherit '(isearch))

  ; Set up which-key support for embark
  (setq embark-indicators
        '(my/embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  (advice-add #'embark-completing-read-prompter
              :around #'my/embark-hide-which-key-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package marginalia
  :demand t
  :init
  (marginalia-mode))


;; Ignore case everywhere
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(setq enable-recursive-minibuffers t)

(use-package savehist
  :config
  (savehist-mode))

(defun my/vertico-avoid-prompt (ret)
  "Advice to avoid selecting the vertico prompt"
  (if (and (alist-get 'vertico--candidates ret)
           (eq (alist-get 'vertico--index ret) -1))
      (setf (alist-get 'vertico--index ret) 0))
  ret)

(use-package vertico
  :demand t

  :config
  (vertico-mode)

  (set-face-attribute 'vertico-current nil :inherit '(highlight underline))
  (advice-add 'vertico--recompute :filter-return #'my/vertico-avoid-prompt))

(use-package vertico-directory
  :ensure nil
  :load-path "straight/build/vertico/extensions" ; vertico-directory is not a real package
  :straight nil
  :init

  (define-key vertico-map "\r" #'vertico-directory-enter)
  (define-key vertico-map "\d" #'vertico-directory-delete-char)
  (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package vertico-multiform
  :ensure nil
  :load-path "straight/build/vertico/extensions"
  :straight nil
  :config
  (vertico-multiform-mode))

(use-package consult
  :demand t

  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  (set-face-attribute 'consult-preview-cursor nil :underline t :inherit nil)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))

  (setq consult-async-refresh-delay .2
      consult-async-input-throttle .4
      consult-async-input-debounce .2
      consult-async-min-input 3))

(use-package consult-ag)

(use-package browse-kill-ring
  :demand t
  :config
  (setq browse-kill-ring-display-duplicates nil
        browse-kill-ring-maximum-display-length 500)

  :bind
  ("M-y" . browse-kill-ring)
  (:map browse-kill-ring-mode-map
         ("C-n" . browse-kill-ring-forward)
         ("C-p" . browse-kill-ring-previous)))

(defun my/override-metadata (overrides ret)
  (if (and (listp ret) (eq (car ret) 'metadata))
      (let ((metadata (cdr ret)))
        (dolist (override overrides)
          (add-to-list 'metadata override))
        `(metadata . ,metadata))
    ret))

;; Helper function to allow overriding the metadata returned from any
;; completion 'table' or other function
(defun my/advice-override-metadata (collection-fn overrides)
  (advice-add collection-fn :filter-return
              `(lambda (ret)
                 (my/override-metadata ',overrides ret))))

(defun my/vertico-prescient--remember ()
  "Advice for remembering candidates in Vertico."
  (when (>= vertico--index 0)
    (prescient-remember
     (substring-no-properties
      (nth vertico--index vertico--candidates)))))

(use-package prescient
  :demand t

  :after (vertico)

  :config
  (prescient-persist-mode)

  ;; Overrides are _added_ to the completion-styles, they do not completely
  ;; replace them. So you probably don't want e.g. flex in there. Also, flex
  ;; completion-style sets the display-sort-function as well if not already
  ;; present. This can result in issues like files not being sorted by the
  ;; most recently accessed. If you would like to use flex but with a different
  ;; sort, you can use the my/advice-override-metadata function to define
  ;; a display-sort-function or specialized completion-category.
  ;; BE CAREFUL when adding anything to 'completion-styles. Inefficient ones
  ;; will cause company completions to be slow
  (setq completion-category-defaults nil
        completion-category-overrides '((multi-category (styles prescient))
                                        (buffer (styles prescient))
                                        (command (styles flex))
                                        (consult-location (styles prescient))
                                        (file (styles prescient partial-completion))))

  (my/advice-override-metadata 'completion-file-name-table
                               '((display-sort-function . prescient-completion-sort)))

  (advice-add 'vertico-insert :after #'my/vertico-prescient--remember))

(provide 'init-minibuffer)
