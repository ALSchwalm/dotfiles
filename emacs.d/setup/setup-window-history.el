;; Setup window history

(defun window-history/push (&rest e)
  (interactive)
  (let ((inhibit-message t))
    (let* ((window (selected-window))
           (push-ring (or (window-parameter window 'window-history/past-ring)
                          (make-ring 20))))
      (ring-insert push-ring (point-marker))
      (set-window-parameter (selected-window) 'window-history/past-ring push-ring))))

(defun window-history/back ()
  (interactive)
  (unless (window-parameter (selected-window) 'window-history/past-ring)
    (error "End of window history"))
  (let* ((window (selected-window))
         (push-ring (window-parameter window 'window-history/past-ring))
         (position (ring-remove push-ring 0)))

    ;; Remove the hook while we switch the buffer
    (remove-hook 'buffer-list-update-hook #'window-history/push t)
    (switch-to-buffer (marker-buffer position))
    (add-hook 'buffer-list-update-hook #'window-history/push t)

    (goto-char position)
    (set-window-parameter (selected-window) 'window-history/past-ring push-ring)
    (recenter)))

;; There is no good hook for this, so try to advice the relevant functions
(advice-add 'push-mark :before #'window-history/push)
(advice-add 'set-mark :before #'window-history/push)

;; Also hook the buffer list change, so we can go back if a command
;; takes us somewhere else.
(add-hook 'buffer-list-update-hook #'window-history/push)

(provide 'setup-window-history)
