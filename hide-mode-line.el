;;; hide-mode-line.el --- minor mode that hides/masks your modeline -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: March 01, 2018
;; Modified: September 21, 2019
;; Version: 1.0.2
;; Keywords: frames mode-line
;; URL: https://github.com/hlissner/emacs-hide-mode-line
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Provides `hide-mode-line-mode`.  A minor mode that hides (or masks) the
;; mode-line in your current buffer.  It can be used to toggle an alternative
;; mode-line, toggle its visibility, or simply disable the mode-line in buffers
;; where it isn't very useful otherwise.
;;
;;; Code:

(defcustom hide-mode-line-format ""
  "The modeline format to use when `hide-mode-line-mode' is active."
  :type 'string
  :group 'hide-mode-line)

(defun hide-mode-line--guess-face ()
  "Set default face to hide mode-line."
  (let ((background (face-background 'default))
        (foreground (face-foreground 'default)))
     (list :box nil :foreground foreground
           :background background :height 0.5)))

(defcustom hide-mode-line-face
  (hide-mode-line--guess-face)
  "Remap to this face when `hide-mode-line-mode' is active."
  :type 'list
  :group 'hide-mode-line)

(defvar-local hide-mode-line--cookies nil
  "Storage for cookies when remaping mode-line and mode-line-inactive faces.")

(defcustom hide-mode-line-excluded-modes '(fundamental-mode)
  "List of major modes where `global-hide-mode-line-mode' won't affect."
  :type 'list
  :group 'hide-mode-line)

(defvar-local hide-mode-line--old-format nil
  "Storage for the old `mode-line-format', so it can be restored when
`hide-mode-line-mode' is disabled.")

(defun hide-mode-line--mask-mode-line ()
  "Mask mode-line.
Use `hide-mode-line-format' and `hide-mode-line-face'."
  (setq-local hide-mode-line--old-format mode-line-format)
  (setq-local hide-mode-line--cookies
              (list (face-remap-add-relative 'mode-line hide-mode-line-face)
                    (face-remap-add-relative 'mode-line-inactive hide-mode-line-face)))
  (setq mode-line-format hide-mode-line-format)
  (force-mode-line-update))

(defun hide-mode-line--unmask-mode-line ()
  "Unmask mode-line.  Revert to original mode-line."
  (setq mode-line-format hide-mode-line--old-format)
  (mapc 'face-remap-remove-relative hide-mode-line--cookies)
  (setq-local hide-mode-line--old-format nil)
  (force-mode-line-update)
  (unless hide-mode-line-format (redraw-display)))

;;;###autoload
(define-minor-mode hide-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if hide-mode-line-mode
      ;; Do not overwrite original mode line
      (unless hide-mode-line--old-format
        (add-hook 'after-change-major-mode-hook
                  #'hide-mode-line-reset nil t)
        (hide-mode-line--mask-mode-line))
    ;; else
    ;; check old-format to prevent setting mode-line-format to nil
    (when hide-mode-line--old-format
      (remove-hook 'after-change-major-mode-hook
                   #'hide-mode-line-reset t)
      (hide-mode-line--unmask-mode-line))))

;; Ensure major-mode or theme changes don't overwrite these variables
(put 'hide-mode-line--old-format 'permanent-local t)
(put 'hide-mode-line-mode 'permanent-local-hook t)
(put 'hide-mode-line-reset 'permanent-local-hook t)

(defun hide-mode-line-reset ()
  "Reset `hide-mode-line-mode' in the current buffer, if necessary.

Sometimes, a major mode is activated after `hide-mode-line-mode' is activated,
thus disabling it (because changing major modes invokes
`kill-all-local-variables' and specifically kills `mode-line-format's local
value, whether or not it's permanent-local.

Attach this to `after-change-major-mode-hook' and `hide-mode-line-mode' will be
cycled to fix this."
  (when hide-mode-line-mode
    (hide-mode-line-mode -1)
    (hide-mode-line-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-hide-mode-line-mode
  hide-mode-line-mode turn-on-hide-mode-line-mode)

;;;###autoload
(defun turn-on-hide-mode-line-mode ()
  "Turn on `hide-mode-line-mode'.
Unless in `fundamental-mode' or `hide-mode-line-excluded-modes'."
  (unless (memq major-mode hide-mode-line-excluded-modes)
    (hide-mode-line-mode +1)))

(provide 'hide-mode-line)
;;; hide-mode-line.el ends here
