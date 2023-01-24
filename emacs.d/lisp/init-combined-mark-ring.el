
(defun my/consult-combined-mark-ring-candidates ()
  (let ((candidates (consult--global-mark-candidates my/combined-mark-ring)))
    (seq-uniq candidates
              (lambda (left right)
                (let ((left-info (get-text-property 0 'consult-location left))
                      (right-info (get-text-property 0 'consult-location right)))
                  (cond ((not (equal (marker-buffer (car left-info))
                                     (marker-buffer (car right-info))))
                         nil)
                        ((not (equal (cdr left-info) (cdr right-info)))
                         nil)
                        (t t)))))))

(defun my/consult-file-prefix (cand)
  (let* ((info (get-text-property 0 'consult-location cand))
         (line (cdr info))
         (buffer-name (buffer-name (marker-buffer (car info)))))
    (list cand (format #("%s:%d " 0 5 (face consult-line-number-prefix))
                       buffer-name line)
          "")))

(defvar my/combined-mark-ring '())
(defvar my/combined-mark-ring-max 50)
(defun my/push-combined-mark (&optional location nomsg activate)
  (when (mark t)
    (let ((old (nth mark-ring-max mark-ring))
          ;; TODO: maybe also dedup on line number?
          (history-delete-duplicates t))
      (add-to-history 'my/combined-mark-ring (copy-marker (mark-marker))
                      my/combined-mark-ring-max t)
      (when old
        (set-marker old nil)))))

(defun my/pop-combined-mark-ring ()
  "Pop off combined mark ring and jump to the top location."
  (interactive)
  ;; Pop entries that refer to non-existent buffers.
  (while (and my/combined-mark-ring (not (marker-buffer (car my/combined-mark-ring))))
    (setq my/combined-mark-ring (cdr my/combined-mark-ring)))
  (or my/combined-mark-ring
      (error "No combined mark set"))
  (let* ((marker (car my/combined-mark-ring))
	 (buffer (marker-buffer marker))
	 (position (marker-position marker)))
    (setq my/combined-mark-ring (cdr my/combined-mark-ring))
    (set-buffer buffer)
    (or (and (>= position (point-min))
	     (<= position (point-max)))
	(if widen-automatically
	    (widen)
	  (error "Combined mark position is outside accessible part of buffer %s"
                 (buffer-name buffer))))
    (goto-char position)
    (switch-to-buffer buffer)))

(defun my/consult-combined-mark-ring ()
  (interactive)
  (consult--read
   (my/consult-combined-mark-ring-candidates)
   :prompt "Go to mark: "
   :annotate #'my/consult-file-prefix
   :sort nil
   :require-match t
   :lookup #'consult--lookup-location
   :history '(:input consult--line-history)
   :add-history (thing-at-point 'symbol)
   :state (consult--jump-state)))

(advice-add 'push-mark :after 'my/push-combined-mark)

(provide 'init-combined-mark-ring)
