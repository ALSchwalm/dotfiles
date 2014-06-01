(defun backward-word-stop-eol (arg)
  (interactive "p")
  (let ((start (point)))
    (save-restriction
      (save-excursion
        (move-beginning-of-line 1)
        (narrow-to-region start (point)))
      (subword-backward arg))))

;; Add basic delete word method
(defun backward-delete-word (arg)
  (interactive "p")
  (if (eq (point) (line-beginning-position))
      (backward-delete-char 1)
    (delete-region (point) (progn (backward-word-stop-eol arg) (point)))))

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

(defun sm-find-tag ()
  (interactive)
  (setq tags-table-list nil)
  (setq tags-file-name (concat (projectile-project-root) "TAGS"))
  (find-tag (funcall (or find-tag-default-function
                         (get major-mode 'find-tag-default-function)
                         'find-tag-default)))
  (recenter-top-bottom))

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

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive)
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
(defun cua-exchange-point-and-mark-center ()
  (interactive)
  (cua-exchange-point-and-mark ())
  (recenter-top-bottom))

(defun delete-whitespace-and-indent ()
  (interactive)
  (let ((start (point)))
    (delete-trailing-whitespace)
    (if (and (not (= start (line-beginning-position)))
             (= (length (buffer-substring (line-beginning-position)
                                          (line-end-position))) 0))
        (indent-for-tab-command))))

(defun project-explorer-toggle ()
  (interactive)
  (if (pe/get-current-project-explorer-buffer)
      (kill-buffer (pe/get-current-project-explorer-buffer))
    (project-explorer-open)))

(defun duplicate-buffer ()
  (interactive)
  (set-window-buffer (next-window) (current-buffer)))

(provide 'functions)
