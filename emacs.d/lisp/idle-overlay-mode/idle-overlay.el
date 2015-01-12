;;; idle-overlay-mode.el --- Apply overlay the word the point is on

;; Copyright (C) 2015 Adam Schwalm

;; Author: Adam Schwalm
;; Based on work by: Phil Hagelberg, Cornelius Mika

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'thingatpt)
(require 'ov)

(defgroup idle-overlay nil
  "Apply an overlay other occurrences of the word at point."
  :group 'faces)

(defcustom idle-overlay-exceptions nil
  "List of words to be excepted from overlaying."
  :group 'idle-overlay
  :type '(repeat string))

(defcustom idle-overlay-idle-time 0.2
  "Time after which to overlay the word at point."
  :group 'idle-overlay
  :type 'float)

(defface idle-overlay-face '((t :weight bold))
  "Face applied to selected symbols."
  :group 'idle-overlay)

(defvar idle-overlay-regexp nil
  "Buffer-local regexp to be overlayed.")

(defvar idle-overlay-global-timer nil
  "Timer to trigger overlaying.")

(defsubst idle-overlay-remove-overlay ()
  "Remove all currently active overlays."
  (ov-clear 'ovl1))

;;;###autoload
(define-minor-mode idle-overlay-mode
  "Idle-Overlay Minor Mode"
  :group 'idle-overlay
  (if (not idle-overlay-mode)
      (idle-overlay-remove-overlay)
    (unless idle-overlay-global-timer
      (setq idle-overlay-global-timer
            (run-with-idle-timer idle-overlay-idle-time
                                 :repeat 'idle-overlay-word-at-point)))
    (set (make-local-variable 'idle-overlay-regexp) nil)))

(defun idle-overlay-word-at-point ()
  "Overlay the word under the point."
  (if (and idle-overlay-mode
           ;; Do not overlay inside of comments for strings
           (not (nth 4 (syntax-ppss)))
           (not (nth 3 (syntax-ppss))))
      (let* ((target-symbol (symbol-at-point))
             (target (symbol-name target-symbol))
             (case-fold-search nil)) ; case sensitive
        (idle-overlay-remove-overlay)
        (when (and target-symbol
                   (looking-at-p (rx (or (syntax symbol) (syntax word)))) ;; Symbol characters
                   (not (member target idle-overlay-exceptions)))
          (ov-set (format (rx word-start "%s" word-end (not (syntax symbol))) target)
                  'face 'idle-overlay-face 'ovl1 t)))))

(provide 'idle-overlay)
;;; idle-overlay.el ends here
