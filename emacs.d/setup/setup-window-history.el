;; Setup window history

(defmacro window-history/--without-advice (body)
  `(progn
     (remove-hook 'buffer-list-update-hook #'window-history/push)
     (advice-remove 'push-mark #'window-history/push)
     (advice-remove 'set-mark #'window-history/push)
     ,body
     (add-hook 'buffer-list-update-hook #'window-history/push)
     (advice-add 'push-mark :before #'window-history/push)
     (advice-add 'set-mark :before #'window-history/push)))


(defun window-history/push (&rest e)
  (interactive)
  (window-history/--without-advice
   (let ((inhibit-message t))
     (let* ((window (selected-window))
            (push-ring (or (window-parameter window 'window-history/past-ring)
                           (make-ring 20)))
            (marker (point-marker)))
       (when (or (ring-empty-p push-ring)
                 (not (equal (ring-ref push-ring 0) marker)))
         (ring-insert push-ring marker)
         (set-window-parameter window 'window-history/past-ring push-ring))))))

;; TODO: don't jump to the same position the point is at
(defun window-history/back ()
  (interactive)
  (window-history/--without-advice
   (let* ((inhibit-message t)
          (window (selected-window))
          (push-ring (window-parameter window 'window-history/past-ring)))
     (if (and push-ring (not (ring-empty-p push-ring)))
         (let* ((position (ring-remove push-ring 0)))
           (unless (equal (current-buffer) (marker-buffer position))
             (switch-to-buffer (marker-buffer position)))
           (goto-char position)

           ;; TODO: avoid recentering when jumping nearby
           (recenter)
           (set-window-parameter window 'window-history/past-ring push-ring))))))

;; There is no good hook for this, so try to advice the relevant functions
(advice-add 'push-mark :before #'window-history/push)
(advice-add 'set-mark :before #'window-history/push)

;; Also hook the buffer list change, so we can go back if a command
;; takes us somewhere else.
(add-hook 'buffer-list-update-hook #'window-history/push)

(provide 'setup-window-history)
