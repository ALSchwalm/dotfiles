
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

(defun my/back-to-indentation-or-beginning ()
  (interactive)
  (if (not visual-line-mode)
      (if (= (point) (progn (back-to-indentation) (point)))
          (beginning-of-line))
    (beginning-of-visual-line)))

(defun my/reset-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-region (point-min) (point-max))
  (insert initial-scratch-message)
  (set-buffer-modified-p nil))

(defun my/new-line-dwim ()
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

(defun my/search-cpp-docs (&optional search)
  "Search en.cppreference.com for a given string"
  (interactive)
  (let ((search (if (not search)
                   (read-from-minibuffer "Search for: ")
                 search))
        (base "http://en.cppreference.com/mwiki/index.php?search="))
    (browse-url (concat base search))
  (message (concat "Search executed using " browse-url-generic-program))))

(require 'thingatpt)
(defun my/search-cpp-symbol-at-point ()
  (interactive)
  (let ((search (thing-at-point 'symbol)))
    (search-cpp-docs search)))

;; Provide a tiling window manager style window movement
(defun my/expand-window-split (&optional delta)
  (interactive)
  (let ((delta (if (not delta) 5 delta)))
    (cond ((window-in-direction 'right)  (shrink-window-horizontally (* -1 delta)))
          ((window-in-direction 'left)   (shrink-window-horizontally delta))
          ((window-in-direction 'above)  (enlarge-window (* -1 delta)))
          ((window-in-direction 'below)  (enlarge-window delta)))))

(defun my/shrink-window-split (&optional delta)
  (interactive)
  (let ((delta (if (not delta) 5 delta)))
    (my/expand-window-split (* -1 delta))))

;; Function to toggle vertical split to horizontal / vice versa
(defun my/toggle-frame-split ()
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil)))

(defun my/transpose-windows (&optional arg)
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
(defun my/refresh-all-buffers ()
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

(defun my/pop-global-mark ()
  (interactive)
  (pop-global-mark)
  (recenter-top-bottom))

(defun my/delete-whitespace-and-indent ()
  (interactive)
  (let ((start (point)))
    (delete-trailing-whitespace)
    (if (and (not (= start (line-beginning-position)))
             (= (length (buffer-substring (line-beginning-position)
                                          (line-end-position))) 0))
        (indent-for-tab-command))))

(defun my/before-save-function ()
  (if (eq major-mode 'c++-mode)
      (clang-format-region (point-min) (point-max)))
  (my/delete-whitespace-and-indent))

(defun my/kill-line ()
  "Kill the remainder of the line, unless the line is only whitespace,
in which case it is deleted."
  (interactive)
  (let ((start (point)))
   (if (not (re-search-forward (rx (not whitespace)) (line-end-position) t))
       (delete-region (point) (1+ (line-end-position)))
     (goto-char start)
     (kill-line))
   (indent-for-tab-command)))

(defun my/duplicate-buffer ()
  (interactive)
  (if (eq (length (window-list)) 1)
      (split-window-right)
    (let ((config (save-window-excursion
                    (delete-other-windows)
                    (window-state-get (selected-window)))))
      (other-window 1)
      (window-state-put config (selected-window)))))

(provide 'functions)
