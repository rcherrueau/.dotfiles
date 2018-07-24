;;; hl-line-*whr.el --- highlight the current line in all buffer  -*- lexical-binding:t -*-

;; Copyright (C) 1998, 2000-2018 Free Software Foundation, Inc.

;; Author:  Ronan-Alexandre Cherrueau <rcherrueau@protonmail.com>
;; Maintainer: rcherrueau@protonmail.com
;; Created: 2018-07-24
;; Keywords: faces, frames, emulations

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An overlay is used.  In the non-sticky cases, this overlay is
;; active only on the selected window.  A hook is added to
;; `post-command-hook' to activate the overlay and move it to the line
;; about point.  To get the non-sticky behavior, `hl-line-*whr-unhighlight'
;; is added to `pre-command-hook' as well.  This function deactivates
;; the overlay unconditionally in case the command changes the
;; selected window.  (It does so rather than keeping track of changes
;; in the selected window).

;; You could make variable `global-hl-line-mode' buffer-local and set
;; it to nil to avoid highlighting specific buffers, when the global
;; mode is used.

;; By default the whole line is highlighted.  The range of highlighting
;; can be changed by defining an appropriate function as the
;; buffer-local value of `hl-line-range-function'.

;;; Code:
(require 'hl-line)
(require 'dash)
(require 'simple)

(defvar hl-line-overlays nil
  "Overlays used by Hl-Line mode to highlight the current line.")

(defvar hl-line-overlay-buffers nil
  "Most recently visited buffers in which Hl-Line-*Whr mode is enabled.")

;;;###autoload
(define-minor-mode hl-line-*whr-mode
  "Toggle highlighting of the current line (Hl-Line-*Whr mode).
With a prefix argument ARG, enable Hl-Line-*Whr mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Hl-Line-*Whr mode is a buffer-local minor mode.  If
`hl-line-sticky-flag' is non-nil, Hl-Line mode highlights the
line about the buffer's point in all windows.  Caveat: the
buffer's point might be different from the point of a
non-selected window.  Hl-Line mode uses the function
`hl-line-*whr-highlight' on `post-command-hook' in this case."
  :group 'hl-line-*whr
  (if hl-line-*whr-mode
      (progn
        ;; In case `kill-all-local-variables' is called.
        (hl-line-*whr-highlight)
        (setq hl-line-overlay-buffers
              (-snoc hl-line-overlay-buffers (current-buffer)))
        ;; Appends hook to execute it after hl-line hook -----v
	      (add-hook 'post-command-hook #'hl-line-*whr-highlight t t))
    (remove-hook 'post-command-hook #'hl-line-*whr-highlight t)
    (setq hl-line-overlay-buffers nil)
    (setq hl-line-overlays nil)
    ))

(defun plist-get-or-else (plist prop else)
  (let ((val (plist-get plist prop)))
    (or val else)))

(defun current-bufferp (buffer)
  (equal buffer (current-buffer)))

(defun hl-line-*whr-highlight ()
  "Activate the Hl-Line overlay of the current line everywhere."
  (unless (overlayp hl-line-overlay)
    (setq hl-line-overlay (car (overlays-at (point))))
    (setq hl-line-overlays (plist-put hl-line-overlays
                                      (current-buffer)
                                      hl-line-overlay)))

  (let* ((ov-props (overlay-properties hl-line-overlay))
         (ov-start (overlay-start hl-line-overlay))
         (ov-line  (line-number-at-pos ov-start)))
    ;; (message "buffers: %s" hl-line-overlay-buffers)
    (save-excursion
    (-each (-remove 'current-bufferp hl-line-overlay-buffers)
      (lambda (buffer)
        (with-current-buffer buffer
          (goto-line ov-line buffer)
          (let ((ov (plist-get-or-else
                       hl-line-overlays
                       buffer
                       (make-overlay (point) (point) buffer))))
            ;; (message "ov-b: %s, ov: %s" buffer ov)
            (setq hl-line-overlays (plist-put hl-line-overlays buffer ov))
            (overlay-put ov 'face (plist-get ov-props 'face))
            (hl-line-move ov))))))))

(provide 'hl-line-*whr)

;;; hl-line-*whr.el ends here
