;; Setup window history

(defvar window-history/saved-window)

(defun window-history/helm-past-ring-candidates ()
  (let ((ring (window-parameter window-history/saved-window 'window-history/past-ring)))
    (unless (null ring)
      (cl-loop with markers = (ring-elements ring)
               for marker in markers
               for m = (helm-global-mark-ring-format-buffer marker)
               collect (cons m marker) into res
               finally return res))))

(defun window-history/helm-past-ring-default-action (candidate)
  (let ((target (copy-marker candidate)))
    (helm-aif (marker-buffer candidate)
        (progn
          (switch-to-buffer it)
          (helm-log-run-hook 'helm-goto-line-before-hook)
          (helm-match-line-cleanup)
          (helm-goto-char target)
          (helm-highlight-current-line))
      (error "Marker points to no buffer"))))

(defvar window-history/helm-source-past-ring
  (helm-build-sync-source "Window History Past Ring"
    :candidates #'window-history/helm-past-ring-candidates
    :action '(("Goto line" . window-history/helm-past-ring-default-action))
    :persistent-help "Goto this location"
    :group 'helm-ring))

(defvar window-history/advice-targets '(isearch-forward
                                        isearch-backward
                                        beginning-of-buffer
                                        end-of-buffer
                                        helm-projectile-rg
                                        my/jump-to-definition-dwim

                                        ;; These should probably be done
                                        ;; via hooks
                                        helm-buffers-list
                                        switch-to-prev-buffer
                                        switch-to-next-buffer))

(defun helm-window-history ()
  (interactive)
  (let ((window-history/saved-window (selected-window)))
    (helm :sources #'window-history/helm-source-past-ring
         :resume 'noresume
         :buffer "*helm window history*")))

(defmacro window-history/--without-advice (body)
  `(progn
     (mapc (lambda (func) (advice-remove func #'window-history/push))
           window-history/advice-targets)
     ,body
     (mapc (lambda (func) (advice-add func :before #'window-history/push))
                      window-history/advice-targets)))

(defun window-history/reset-past-ring ()
  (let* ((window (selected-window))
         (new-ring (make-ring 20)))
    (set-window-parameter window 'window-history/past-ring new-ring)))

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

(defun window-history/back ()
  (interactive)
  (window-history/--without-advice
   (let* ((inhibit-message t)
          (window (selected-window))
          (push-ring (window-parameter window 'window-history/past-ring)))
     (if (and push-ring (not (ring-empty-p push-ring)))
         (let ((position (ring-remove push-ring 0)))

           ;; Find the most recent location that isn't the current one
           (while (and (not (ring-empty-p push-ring))
                       (equal position (point-marker)))
             (setq position (ring-remove push-ring 0)))

           (if (not (equal position (point-marker)))
               (progn
                 (unless (equal (current-buffer) (marker-buffer position))
                   (switch-to-buffer (marker-buffer position)))
                 (goto-char position)

                 ;; TODO: avoid recentering when jumping nearby
                 (recenter)))
           (set-window-parameter window 'window-history/past-ring push-ring))))))


;; TODO: this should activated by a global minor mode
(mapc (lambda (func) (advice-add func :before #'window-history/push))
      window-history/advice-targets)

(provide 'setup-window-history)
