(defvar my/backward-word-stop-regex
     (rx
      (or ?\" "}" "{" "<" ">" "[" "]" "(" ")")))

(defun my/backward-word-stop (arg)
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
                         my/backward-word-stop-regex
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
(defun my/backward-delete-word (arg)
  (interactive "p")
  (if (= (point) (line-beginning-position))
      (backward-delete-char 1)
    (delete-region (point) (progn (my/backward-word-stop arg) (point)))))

(defun my/helm-projectile-find-file-with-fallback ()
  (interactive)
  (condition-case nil
      (helm-projectile-find-file)
    (error (ido-find-file))))

(defun my/helm-quit-and-ido-find-file ()
  (interactive)
  (helm-quit-and-execute-action 'ido-find-file))

(defun reset-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-region (point-min) (point-max))
  (insert initial-scratch-message)
  (set-buffer-modified-p nil))

(defun my/duplicate-buffer ()
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun my/delete-whitespace-and-indent ()
  (interactive)
  (let ((start (point)))
    (delete-trailing-whitespace)
    (if (and (not (= start (line-beginning-position)))
             (= (length (buffer-substring (line-beginning-position)
                                          (line-end-position))) 0))
        (indent-for-tab-command))))

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

(defun my/before-save-function ()
  (if (eq major-mode 'c++-mode)
      (clang-format-region (point-min) (point-max)))
  (if (eq major-mode 'rust-mode)
      (rust-format-buffer))
  (my/delete-whitespace-and-indent))

(defun my/save-without-trim ()
  (interactive)
  (let ((b (current-buffer)))
    (with-temp-buffer
      (let ((before-save-hook (remove 'my/before-save-function before-save-hook)))
        (with-current-buffer b
          (let ((before-save-hook (remove 'my/before-save-function before-save-hook)))
            (save-buffer)))))))

(provide 'functions)
