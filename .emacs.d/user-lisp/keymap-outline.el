;;; keymap-outline.el --- Show active key bindings folded by prefix -*- lexical-binding: t -*-

;; Author: naokton <march.of.rabbits@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: help

;;; Commentary:

;; `keymap-outline-show' lists all key bindings active in the current
;; buffer, and the key translations in `key-translation-map' (such as
;; \\`C-x 8').
;;
;; The list is folded by prefix key with `outline-minor-mode'.  In the
;; output buffer:
;;
;;   TAB, RET or mouse-1 in the margin on a heading
;;               show the prefix's own keys, or hide its subtree
;;   S-TAB       cycle the whole buffer
;;   RET         describe the command on the current line
;;   n, p        move to the next or previous line
;;   C-c C-n, C-c C-p
;;               move to the next or previous visible heading
;;   C-c C-f, C-c C-b
;;               move to the next or previous heading at the same level
;;   C-c C-u     move up to the parent heading
;;   g           collect the bindings again

;;; Code:

(require 'cl-lib)
(require 'outline)

(defgroup keymap-outline nil
  "Show active key bindings folded by prefix."
  :group 'help)

(defcustom keymap-outline-ignore-regexp
  (rx bos (or "A-" "<help>"
              (seq (? "S-") "<" (or "dead-" "mute-"))
              (seq "<" (or "header-line" "mode-line" "tab-bar" "tool-bar"
                           "left-" "right-"))))
  "Regexp matching key descriptions to leave out of the list.
It is matched against the output of `key-description', such as
\"<f1> a\".  A match on a prefix key leaves out all keys under it.
The default drops iso-transl's dead-key and Alt
translations (unused on macOS; \\`C-x 8' is kept), \"<help>\" (same
as \"<f1>\"), and mouse events on screen areas other than the text."
  :type '(choice (const :tag "None" nil) regexp))

(defconst keymap-outline--ignored-commands
  '(self-insert-command ignore ignore-event undefined)
  "Definitions that are not listed.
They still hide the bindings of the same key in lower keymaps.")

(defvar-local keymap-outline--source nil
  "Buffer whose key bindings `keymap-outline-mode' shows.")

(defun keymap-outline--bindings (map &optional prefix)
  "Return the bindings in MAP as a list of (KEY . DEFINITION).
KEY is the `key-description' of PREFIX, a vector, followed by the
key in MAP.  Keys matching `keymap-outline-ignore-regexp' are left
out.  When MAP is composed of several keymaps, a binding in one hides
the bindings of the same key in the keymaps after it, as in the
active keymaps."
  (let (seen bindings)
    (map-keymap
     (lambda (event def)
       ;; A char-table range such as (32 . 126) stands for each char in
       ;; it.  Skip huge `self-insert-command' ranges before expanding.
       (dolist (event (if (consp event)
                          (unless (memq def keymap-outline--ignored-commands)
                            (number-sequence (car event) (cdr event)))
                        (list event)))
         (unless (or (memq event seen)
                     (and (symbolp event)
                          (string-match-p
                           (rx (or "mouse-" "wheel-" "drag-" "scroll-bar"
                                   "select-window" "switch-frame"
                                   "remap" "menu-bar"))
                           (symbol-name event))))
           (push event seen)
           (let* ((key (vconcat prefix (list event)))
                  (desc (key-description key))
                  ;; `lookup-key' finds the binding that takes effect: it
                  ;; resolves menu items, and merges the prefix keymaps of
                  ;; the same key unless a keymap before binds a command.
                  (def (lookup-key map (vector event))))
             (cond ((and keymap-outline-ignore-regexp
                         (string-match-p keymap-outline-ignore-regexp desc)))
                   ((keymapp def)
                    (setq bindings (nconc (keymap-outline--bindings def key)
                                          bindings)))
                   ((and def (not (memq def keymap-outline--ignored-commands)))
                    (push (cons desc def) bindings)))))))
     map)
    bindings))

(defun keymap-outline--rows ()
  "Return active bindings in the current buffer as rows.
Each row is (PREFIX KEY DEFINITION), where PREFIX is the list of
keys before the last one in KEY."
  (cl-loop for (key . def) in (keymap-outline--bindings
                               (make-composed-keymap
                                (append (current-active-maps t)
                                        (list key-translation-map))))
           collect (list (or (butlast (split-string key " ")) '("(top level)"))
                         key def)))

(defun keymap-outline--revert (&rest _)
  "Render the bindings of `keymap-outline--source'."
  (unless (buffer-live-p keymap-outline--source)
    (user-error "Source buffer was killed"))
  (let* (;; Sorting by prefix first puts each prefix's own keys before
         ;; its sub-prefixes, as outline headings need.
         (rows (sort (with-current-buffer keymap-outline--source
                       (keymap-outline--rows))
                     :key (lambda (r) (list (car r) (cadr r)))))
         (width (apply #'max 0 (mapcar (lambda (r)
                                         (+ (* 2 (length (car r)))
                                            (string-width (cadr r))))
                                       rows)))
         (inhibit-read-only t)
         path)
    (erase-buffer)
    (pcase-dolist (`(,prefix ,key ,def) rows)
      ;; Insert headings for the prefixes not shown yet.
      (cl-loop for level from (1+ (or (cl-mismatch path prefix :test #'equal)
                                      (length prefix)))
               to (length prefix)
               do (insert (propertize
                           (concat (make-string (* 2 (1- level)) ?\s)
                                   (string-join (take level prefix) " "))
                           'face (aref outline-font-lock-faces
                                       (% (1- level) (length outline-font-lock-faces)))
                           'outline-level level)
                          "\n"))
      (setq path prefix)
      (let ((indent (* 2 (length prefix))))
        (insert (propertize
                 (concat (make-string indent ?\s)
                         (propertize key 'face 'help-key-binding)
                         (make-string (- width indent (string-width key)) ?\s)
                         " → "
                         (cond ((symbolp def)
                                (propertize (symbol-name def)
                                            'face 'font-lock-function-name-face))
                               ;; A keyboard macro or a key translation.
                               ((or (stringp def) (vectorp def))
                                (propertize (key-description def)
                                            'face 'help-key-binding))
                               ;; Such as `kmacro' objects.
                               (t (symbol-name (or (oclosure-type def) 'lambda)))))
                 'keymap-outline-command (and (symbolp def) def))
                "\n")))
    (goto-char (point-min))
    (outline-hide-sublevels 1)))

(defun keymap-outline-toggle (&optional event)
  "Show the heading's own keys and sub-headings, or hide its subtree.
If EVENT is a mouse event, act on the heading clicked."
  (interactive (list last-nonmenu-event))
  (save-excursion
    (when (mouse-event-p event)
      (mouse-set-point event))
    (outline-toggle-children)))

(defun keymap-outline-describe ()
  "Toggle the heading at point, or describe the command on this line."
  (interactive)
  (let ((command (get-text-property (pos-bol) 'keymap-outline-command)))
    (cond ((outline-on-heading-p) (outline-toggle-children))
          (command (describe-function command))
          (t (message "No command on this line")))))

(defvar-keymap keymap-outline-mode-map
  "RET" #'keymap-outline-describe
  "n" #'next-line
  "p" #'previous-line
  ;; Heading motion with the same keys as Org mode.
  "C-c C-n" #'outline-next-visible-heading
  "C-c C-p" #'outline-previous-visible-heading
  "C-c C-f" #'outline-forward-same-level
  "C-c C-b" #'outline-backward-same-level
  "C-c C-u" #'outline-up-heading
  ;; `outline-cycle' (TAB and margin clicks) hides the heading's own keys
  ;; in its "headings only" state.
  "<remap> <outline-cycle>" #'keymap-outline-toggle)

(define-derived-mode keymap-outline-mode special-mode "Keymap-Outline"
  "Major mode for `keymap-outline-show' output.
\\{keymap-outline-mode-map}"
  (setq-local revert-buffer-function #'keymap-outline--revert
              outline-search-function #'outline-search-level
              outline-level (lambda () (get-text-property (point) 'outline-level))
              outline-minor-mode-cycle t
              outline-minor-mode-use-buttons 'in-margins)
  (outline-minor-mode 1))

;;;###autoload
(defun keymap-outline-show ()
  "Show all key bindings active in the current buffer, folded by prefix."
  (interactive)
  (let ((source (current-buffer))
        (buf (get-buffer-create "*keymap-outline*")))
    (with-current-buffer buf
      (keymap-outline-mode)
      (setq keymap-outline--source source)
      (revert-buffer))
    (pop-to-buffer buf)))

(provide 'keymap-outline)
;;; keymap-outline.el ends here
