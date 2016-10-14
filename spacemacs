;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/.spacemacs-config/")
   dotspacemacs-configuration-layers
   '(
     python
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     (c-c++ :variables
            c-c++-enable-clang-support t
            c-c++-default-mode-for-headers 'c++-mode)
     csharp
     emacs-lisp
     git
     haskell
     html
     rust
     react
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'ansi-term
            shell-default-term-shell "/usr/sbin/zsh")
     spell-checking
     (syntax-checking :variables
                      syntax-checking-enable-tooltips nil)
     version-control
     alschwalm
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(rainbow-delimiters highlight-parentheses)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-editing-style 'emacs
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-themes '(solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro-13"
                               :powerline-scale 1.1)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize t
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header t
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.6
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-command-key ":"
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  (set-default 'truncate-lines t)
  (setq vc-follow-symlinks t))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (unless (server-running-p)
    (server-start))

  (global-company-mode)
  (global-auto-complete-mode)
  (setq auto-window-vscroll nil)

  (smartparens-global-mode -1)
  (global-flycheck-mode)
  (global-vi-tilde-fringe-mode -1)
  (global-subword-mode)

  (add-hook 'before-save-hook 'my/before-save-function)

  (helm-projectile-on)
  (define-key helm-projectile-find-file-map (read-kbd-macro "C-f")
    'my/helm-quit-and-ido-find-file)

  (define-key projectile-command-map "s" 'helm-projectile-ag)

  (setq-default cursor-type 'bar)
  (setq evil-emacs-state-cursor '((bar . 2)))
  (blink-cursor-mode)
  (set-default 'cursor-in-non-selected-windows nil)

  (setq flycheck-idle-change-delay 2
        flycheck-check-syntax-automatically '(save new-line mode-enabled)
        flycheck-clang-include-path (quote ("/usr/include" "/usr/include/python2.7" )))

  ;; Enable better c++
  (add-hook 'c++-mode-hook (lambda()
                             (setq flycheck-clang-language-standard "c++1y")))

  (add-hook 'rust-mode-hook #'rustfmt-enable-on-save)

  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))



;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-toggle-key "")
 '(js-indent-level 2)
 '(org-startup-indented t)
 '(package-selected-packages
   (quote
    (wgrep smex counsel-projectile counsel swiper pug-mode yapfify uuidgen toc-org py-isort org org-plus-contrib org-bullets mwim livid-mode skewer-mode simple-httpd live-py-mode link-hint intero hlint-refactor helm-hoogle git-link flyspell-correct-helm flyspell-correct seq eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff eshell-z dumb-jump company-ghci column-enforce-mode cargo omnisharp csharp-mode spinner json-snatcher json-reformat parent-mode request haml-mode gitignore-mode fringe-helper git-gutter+ epl flx highlight web-completion-data dash-functional pos-tip pythonic s bind-map xterm-color ws-butler window-numbering web-mode toml-mode spacemacs-theme spaceline racer rust-mode persp-mode orgit open-junk-file neotree magit-gitflow leuven-theme less-css-mode js2-refactor js2-mode indent-guide hl-todo hindent help-fns+ helm-themes helm-pydoc helm-projectile helm-make projectile helm-descbinds helm-c-yasnippet helm-ag haskell-snippets google-translate git-messenger flycheck-rust expand-region exec-path-from-shell evil-surround evil-search-highlight-persist evil-mc evil-matchit evil-magit magit magit-popup evil-iedit-state iedit evil-exchange eshell-prompt-extras emmet-mode diff-hl company-racer company-quickhelp company-anaconda cmake-mode clang-format auto-yasnippet yasnippet auto-compile anaconda-mode ace-link ace-jump-helm-line auto-complete avy ghc tern anzu smartparens haskell-mode flycheck git-gutter git-commit with-editor company helm helm-core hydra f quelpa package-build use-package which-key evil dash web-beautify volatile-highlights vi-tilde-fringe undo-tree tagedit solarized-theme smooth-scrolling smeargle slim-mode shm shell-pop scss-mode sass-mode restart-emacs pyvenv pytest pyenv-mode py-yapf powerline popwin popup pkg-info pip-requirements pcre2el paredit paradox page-break-lines packed multiple-cursors multi-term move-text macrostep lorem-ipsum linum-relative key-chord json-mode js-doc jade-mode info+ ido-vertical-mode hy-mode hungry-delete highlight-numbers highlight-indentation helm-swoop helm-mode-manager helm-gitignore helm-flyspell helm-flx helm-css-scss helm-company goto-chg golden-ratio gitconfig-mode gitattributes-mode git-timemachine git-gutter-fringe git-gutter-fringe+ flycheck-pos-tip flycheck-haskell flx-ido fill-column-indicator fancy-battery evil-visualstar evil-tutor evil-numbers evil-nerd-commenter evil-lisp-state evil-indent-plus evil-escape evil-args evil-anzu eval-sexp-fu esh-help elisp-slime-nav disaster diminish define-word deferred cython-mode company-web company-tern company-statistics company-ghc company-cabal company-c-headers coffee-mode cmm-mode clean-aindent-mode buffer-move bracketed-paste bind-key auto-highlight-symbol auto-dictionary async aggressive-indent adaptive-wrap ace-window ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
