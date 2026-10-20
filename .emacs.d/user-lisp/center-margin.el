;;; center-margin.el --- Center text in windows with margins -*- lexical-binding: t -*-

;; Author: naokton <march.of.rabbits@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience

;;; Commentary:

;; `center-margin-mode' centers the text of the current buffer in each
;; window that shows it, by setting the window margins so that the
;; text area is `center-margin-width' columns wide.  Windows narrower
;; than that get no margins.
;;
;; `global-center-margin-mode' turns it on in all buffers except
;; internal ones (whose names start with a space, such as minibuffers
;; and the echo area).  Use `global-center-margin-modes' to leave out
;; major modes.  `center-margin-mode' can still toggle it in any buffer.
;;
;; `center-margin-widen', `center-margin-narrow' and
;; `center-margin-reset' change the width for the current buffer.
;; With `repeat-mode', repeat them with {, } and 0.
;;
;; When `window-splittable-p' checks whether a centered window can be
;; split side by side, it measures the window without its margins.

;;; Code:

(defgroup center-margin nil
  "Center text in windows with margins."
  :group 'convenience)

(defcustom center-margin-width 128
  "Width of the centered text area, in columns.
`center-margin-widen' and `center-margin-narrow' set it for the
current buffer only."
  :type 'natnum)

(defcustom center-margin-step 8
  "Number of columns that `center-margin-widen' adds per step."
  :type 'natnum)

(defun center-margin--available-width (window)
  "Return the width of the text area of WINDOW without margins.
Remove the margins of WINDOW if they do not fit in it."
  ;; The body width is 0 when the margins do not fit, as right after
  ;; a split.
  (when (zerop (window-body-width window))
    (set-window-margins window 0 0))
  (let ((margins (window-margins window)))
    (+ (window-body-width window) (or (car margins) 0) (or (cdr margins) 0))))

(defun center-margin--update (window)
  "Set the margins of WINDOW to center its text area."
  (let ((margin (max 0 (/ (- (center-margin--available-width window)
                             center-margin-width)
                          2))))
    ;; Let `split-window' split the window regardless of its margins.
    ;; This function sets the margins again for the new size.
    (set-window-parameter window 'min-margins '(0 . 0))
    (set-window-margins window margin margin)))

(defun center-margin--update-windows ()
  "Center the text in each window that shows the current buffer."
  (dolist (window (get-buffer-window-list nil nil t))
    (center-margin--update window)))

;;;###autoload
(define-minor-mode center-margin-mode
  "Center the text in each window that shows this buffer."
  :group 'center-margin
  (if center-margin-mode
      (progn
        (add-hook 'window-size-change-functions #'center-margin--update nil t)
        (center-margin--update-windows))
    (remove-hook 'window-size-change-functions #'center-margin--update t)
    (dolist (window (get-buffer-window-list nil nil t))
      (set-window-parameter window 'min-margins nil)
      (set-window-margins window left-margin-width right-margin-width))))

(defun center-margin--turn-on ()
  "Turn on `center-margin-mode' unless the buffer is an internal one."
  (unless (string-prefix-p " " (buffer-name))
    (center-margin-mode)))

;;;###autoload
(define-globalized-minor-mode global-center-margin-mode
  center-margin-mode center-margin--turn-on
  :group 'center-margin
  :predicate t)

(defun center-margin-widen (n)
  "Widen the centered text area of the current buffer by N steps.
Each step is `center-margin-step' columns.  The width stays within the
selected window.  Turn on `center-margin-mode' if it is off."
  (interactive "p")
  (let* ((available (center-margin--available-width (selected-window)))
         ;; Start from the visible width when the window is narrower.
         (width (+ (min center-margin-width available) (* n center-margin-step))))
    (setq-local center-margin-width (min available (max center-margin-step width))))
  (if center-margin-mode
      (center-margin--update-windows)
    (center-margin-mode))
  (message "Text width: %d" center-margin-width))

(defun center-margin-narrow (n)
  "Narrow the centered text area of the current buffer by N steps.
See `center-margin-widen'."
  (interactive "p")
  (center-margin-widen (- n)))

(defun center-margin-reset ()
  "Reset the centered text width of the current buffer to the default."
  (interactive)
  (kill-local-variable 'center-margin-width)
  (when center-margin-mode
    (center-margin--update-windows))
  (message "Text width: %d" center-margin-width))

(defvar-keymap center-margin-repeat-map
  :doc "Keymap to repeat `center-margin-widen' and related commands."
  :repeat t
  "{" #'center-margin-narrow
  "}" #'center-margin-widen
  "0" #'center-margin-reset)

(defun center-margin--splittable-p (orig window &optional horizontal)
  "Call ORIG with WINDOW and HORIZONTAL, measuring WINDOW without margins.
Do this only for a side-by-side split of a window whose buffer has
`center-margin-mode' on."
  (if (and horizontal
           (buffer-local-value 'center-margin-mode (window-buffer window)))
      (let ((margins (window-margins window)))
        (set-window-margins window 0 0)
        (unwind-protect
            (funcall orig window horizontal)
          (set-window-margins window (car margins) (cdr margins))))
    (funcall orig window horizontal)))

(advice-add 'window-splittable-p :around #'center-margin--splittable-p)

(provide 'center-margin)

;;; center-margin.el ends here
