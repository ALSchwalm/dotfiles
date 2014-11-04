;;; This file is reproduced in part from the nxhtml project.
;;; Specifically, the file ourcomments-util.el available at:
;;; https://github.com/emacsmirror/nxhtml/blob/master/util/ourcomments-util.el
;;
;; Author: Lennart Borgman <lennart dot borgman at gmail dot com>
;; Created: Wed Feb 21 2007

;; Last-Updated: 2010-05-29 Sat
;; Keywords:
;; Compatibility: Emacs 22
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `cus-edit', `cus-face', `cus-load',
;;   `cus-start', `wid-edit'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; The functionality given by these small routines should in my
;; opinion be part of Emacs (but they are not that currently).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'thingatpt)
(defun ourcomments-bounds-of-string-at-point ()
  "Return bounds of string at point if any."
  (ourcomments-string-or-comment-bounds-1 'string))
(put 'string 'bounds-of-thing-at-point 'ourcomments-bounds-of-string-at-point)


(defun ourcomments-bounds-of-comment-at-point ()
  "Return bounds of comment at point if any."
  (ourcomments-string-or-comment-bounds-1 'comment))
(put 'comment 'bounds-of-thing-at-point 'ourcomments-bounds-of-comment-at-point)


(defun ourcomments-bounds-of-string-or-comment-at-point ()
  "Return bounds of string or comment at point if any."
  (ourcomments-string-or-comment-bounds-1 nil))
(put 'string-or-comment 'bounds-of-thing-at-point 'ourcomments-bounds-of-string-or-comment-at-point)

(defun ourcomments-string-or-comment-bounds-1 (what)
  (save-restriction
    (widen)
    (let* ((here (point))
           ;; Fix-me: when on end-point, how to handle that and which should be last hit point?
           (state (parse-partial-sexp (point-min) (1+ here)))
           (type (if (nth 3 state)
                     'string
                   (if (nth 4 state)
                       'comment)))
           (start (when type (nth 8 state)))
           end)
      (unless start
        (setq state (parse-partial-sexp (point-min) here))
        (setq type (if (nth 3 state)
                       'string
                     (if (nth 4 state)
                         'comment)))
        (setq start (when type (nth 8 state))))
      (unless (or (not what)
                  (eq what type))
        (setq start nil))
      (if (not start)
          (progn
            (goto-char here)
            nil)
        (setq state (parse-partial-sexp (1+ start) (point-max)
                                        nil nil state 'syntax-table))
        (setq end (point))
        (goto-char here)
        (cons start end)))))

(provide 'thing-at-point-string)
