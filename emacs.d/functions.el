(require 'dash)

(defvar backward-word-stop-regex
     (rx
      (or ?\" "}" "{" "<" ">" "[" "]" "(" ")")))

(defun backward-word-stop (arg)
  (interactive "p")
  (let ((start (point))
        (start-of-line (line-beginning-position))
        (subword-back nil)
        (search-back nil))
    (save-excursion
      (subword-backward arg)
      (setq subword-back (point)))
    (save-excursion
      (setq search-back (re-search-backward
                         backward-word-stop-regex
                         subword-back t)))
    (cond
     ;; If the point is at a word stop, move back one char
     ((equal search-back (- (point) 1))
      (backward-char 1))

     ;; If the regex word stop finds something on the same line,
     ;; move to that point
     ((and search-back (>= search-back start-of-line))
      (goto-char search-back)
      (forward-char))

     ;; If the regex finds something on a different line, move to
     ;; the end of that line
     ((and search-back (< search-back start-of-line))
      (goto-char search-back)
      (move-end-of-line 1))

     ;; If the subword delete takes the point up a line, move to the
     ;; end of that line
     ((< subword-back start-of-line)
      (subword-backward arg)
      (move-end-of-line 1))

     ;; Otherwise, do the subword-backward
     ((not (< subword-back start-of-line))
      (subword-backward arg)))))

;; Add basic delete word method
(defun backward-delete-word (arg)
  (interactive "p")
  (if (= (point) (line-beginning-position))
      (backward-delete-char 1)
    (delete-region (point) (progn (backward-word-stop arg) (point)))))

(defun paredit-beginning-of-sexp ()
  (interactive)
  (paredit-close-round)
  (beginning-of-sexp))

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (not visual-line-mode)
      (if (= (point) (progn (back-to-indentation) (point)))
          (beginning-of-line))
    (beginning-of-visual-line)))

(defun mc/mark-next-like-this-expand ()
  (interactive)
  (if (not (region-active-p))
      (er/expand-region 1))
  (mc/mark-next-like-this 1))

(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                             (and (looking-back ">" 1) (looking-at "<"))
                             (and (looking-back "(" 1) (looking-at ")"))
                             (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

(defun search-cpp-docs (&optional search)
  "Search en.cppreference.com for a given string"
  (interactive)
  (let ((search (if (not search)
                   (read-from-minibuffer "Search for: ")
                 search))
        (base "http://en.cppreference.com/mwiki/index.php?search="))
    (browse-url (concat base search))
  (message (concat "Search executed using " browse-url-generic-program))))

(require 'thingatpt)
(defun search-cpp-symbol-at-point ()
  (interactive)
  (let ((search (thing-at-point 'symbol)))
    (search-cpp-docs search)))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun test-file (&optional number)
  (interactive "P")
  (let* ((current (file-name-extension (buffer-name)))
         (extension (read-string "Test file format (e.g. py, d, cpp): " current))
         (number (if (not number)
                     (string-to-number (read-string "Test file number: "))
                   number)))
    (find-file (concat "~/test/test" (number-to-string number) "." extension))))

;; Provide a tiling window manager style window movement
(defun expand-window-split (&optional delta)
  (interactive)
  (let ((delta (if (not delta) 5 delta)))
    (cond ((window-in-direction 'right)  (shrink-window-horizontally (* -1 delta)))
          ((window-in-direction 'left)   (shrink-window-horizontally delta))
          ((window-in-direction 'above)  (enlarge-window (* -1 delta)))
          ((window-in-direction 'below)  (enlarge-window delta)))))

(defun shrink-window-split (&optional delta)
  (interactive)
  (let ((delta (if (not delta) 5 delta)))
    (expand-window-split (* -1 delta))))

;; Function to toggle vertical split to horizontal / vice versa
(defun toggle-frame-split ()
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil)))

(defun transpose-windows (&optional arg)
  "Transpose the buffers shown in two windows."
  (interactive)
  (setq arg (or arg 1))
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; Reload files (for after a pull)
(defun refresh-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))

;; Shorthand for C-x C-x then C-l
(defun exchange-point-and-mark-center ()
  (interactive)
  (exchange-point-and-mark ())
  (deactivate-mark)
  (recenter-top-bottom))

(defun delete-whitespace-and-indent ()
  (interactive)
  (let ((start (point)))
    (delete-trailing-whitespace)
    (if (and (not (= start (line-beginning-position)))
             (= (length (buffer-substring (line-beginning-position)
                                          (line-end-position))) 0))
        (indent-for-tab-command))))

(defun my-before-save-function ()
  (if (eq major-mode 'c++-mode)
      (clang-format-region (point-min) (point-max)))
  (delete-whitespace-and-indent))

(defun my-kill-line ()
  "Kill the remainder of the line, unless the line is only whitespace,
in which case it is deleted."
  (interactive)
  (let ((start (point)))
   (if (not (re-search-forward (rx (not whitespace)) (line-end-position) t))
       (delete-region (point) (1+ (line-end-position)))
     (goto-char start)
     (kill-line))
   (indent-for-tab-command)))

(defun inside-comment-p ()
  "Returns non-nil if inside comment, else nil.
This depends on major mode having setup syntax table properly."
  (nth 4 (syntax-ppss)))

(defun inside-string-p ()
  "Returns non-nil if inside string, else nil.
This depends on major mode having setup syntax table properly."
  (nth 3 (syntax-ppss)))

(defun project-explorer-toggle ()
  (interactive)
  (if (pe/get-current-project-explorer-buffer)
      (kill-buffer (pe/get-current-project-explorer-buffer))
    (project-explorer-open)))

(defun duplicate-buffer ()
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun commment-indent-region ()
  "Indent all comments in the region to the comment-column. Comments
on their own line will not be indented."
  (interactive)
  (save-excursion
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (while (re-search-forward comment-start (mark) t)
      (backward-char (length comment-start))
      (let ((start (point)))
        (back-to-indentation)
        (if (not (eq (point) start))
            (comment-indent)))
      (forward-char (length comment-start))))
  (deactivate-mark))

(require 'find-file "find-file")
(defun find-other-buffer ()
  "ff-find-other-file with buffers"
  (let ((other-buffer nil)
        (extension-list (cadr (-first (lambda (pair)
                                        (string= (car pair) (format "\\.%s\\'" (file-name-extension (buffer-file-name)))))
                                      cc-other-file-alist))))
    (-first (lambda (buffer)
              (-first (lambda (extension)
                        (if (string-match
                             (concat (file-name-sans-extension (buffer-name)) extension)
                             (buffer-name buffer))
                            (setq other-buffer buffer))) extension-list))
            (buffer-list))
    other-buffer))

(provide 'functions)
