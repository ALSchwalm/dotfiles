

;; Make compile buffer show lines
(defun truncate-hook()
  (setq truncate-lines nil))
(add-hook 'compilation-mode-hook 'truncate-hook)

;; Fix haskell mode indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))

(defun ansi-term-default ()
  (interactive)
  (ansi-term "zsh" "localhost"))

(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)
                            (setq global-hl-line-mode nil)))

;; Remove whitespace
(add-hook 'before-save-hook 'my-before-save-function)

;; Better defaults for Markdown mode
(add-hook 'markdown-mode-hook (lambda()
                                (visual-line-mode t)
                                (setq truncate-lines nil)
                                (setq word-wrap t)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Spell checking in text-based modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(add-hook 'c++-mode-hook
          '(lambda()
             (local-set-key (read-kbd-macro "<f1>") 'search-cpp-symbol-at-point)

             ;; We could place some regexes into `c-mode-common-hook', but note that their evaluation order
             ;; matters.
             (font-lock-add-keywords
              nil '(;; complete some fundamental keywords
                    ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
                    ;; namespace names and tags - these are rendered as constants by cc-mode
                    ("\\<\\(\\w+::\\)" . font-lock-function-name-face)

                    ;; Some support for c++11 variadic templates
                    ("\\<typename[[:blank:]]*\\.\\.\\.[[:blank:]]+\\([A-Z_a-z]+\\)" 1 font-lock-type-face)

                    ;;  new C++11 keywords
                    ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
                    ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
                    ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
                    ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
                    ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
                    ;; hexadecimal numbers
                    ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
                    ;; integer/float/scientific numbers
                    ("\\<[-+]?[0-9]*\\.?[0-9]+\\([uUlL]+\\|[eE][-+]?[0-9]+\\)?[fFlL]?\\>" . font-lock-constant-face)
                    ;; c++11 string literals
                    ;;       L"wide string"
                    ;;       L"wide string with UNICODE codepoint: \u2018"
                    ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
                    ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
                    ;;       R"(user-defined literal)"
                    ;;       R"( a "quot'd" string )"
                    ;;       R"delimiter(The String Data" )delimiter"
                    ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
                    ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
                    (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string
                    (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

                    ;; user-defined types (rather project-specific)
                    ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
                    ("[^a-zA-Z0-9_)]\\(\\[\\).*\\]*?\\(\\]\\)[ \t]*(.*)" (1 font-lock-function-name-face) (2 font-lock-function-name-face))
                    ))
             ) t)

(add-hook 'prog-mode-hook
          '(lambda ()
             (define-key (current-local-map) (read-kbd-macro "<M-backspace>") 'backward-kill-sexp)
             (font-lock-add-keywords
              nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)\\>"
                     1 '((:weight bold)) t)))))

(add-hook 'c-initialization-hook
          '(lambda ()
             (define-key c-mode-base-map (read-kbd-macro "<C-backspace>") 'backward-delete-word)))

;(add-hook 'python-mode-hook 'jedi:setup)

(add-hook 'ecb-activate-hook #'(lambda ()
                                 (popwin-mode -1)))
(add-hook 'ecb-deactivate-hook #'(lambda ()
                                   (popwin-mode t)))

; Create any missing directory when using find-file
(defun create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist. Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'create-non-existent-directory)

;; Auto-update smex
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)

(provide 'setup-hooks)
