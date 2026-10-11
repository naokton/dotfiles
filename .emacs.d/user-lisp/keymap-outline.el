;;; keymap-outline.el --- Show active key bindings folded by prefix -*- lexical-binding: t -*-

;; Author: naokton <march.of.rabbits@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: help

;;; Commentary:

;; `keymap-outline-show' lists all key bindings active in the current
;; buffer, and the key translations in `key-translation-map' (such as
;; \\`C-x 8').  `keymap-outline-show-free' shows instead which keys
;; are free under each prefix key, grouped by modifier.  With a prefix
;; argument, it reads major modes (all those of live buffers by
;; default) and shows which keys are free in all their live buffers,
;; and which in some.
;;
;; The list is folded by prefix key with `outline-minor-mode'.  In the
;; output buffer:
;;
;;   TAB, RET or mouse-1 in the margin on a heading
;;               show the prefix's own keys, or hide its subtree
;;   S-TAB       cycle the whole buffer
;;   RET         describe the command on the current line, or the key
;;               at point in `keymap-outline-show-free' (or its binding
;;               in each mode)
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

(defcustom keymap-outline-ignored-modes nil
  "Major modes that `keymap-outline-show-free' leaves out of all modes."
  :type '(repeat (symbol :tag "Major mode")))

(defcustom keymap-outline-free-modifiers
  '(nil (control) (meta) (control meta))
  "Modifier groups of the keys `keymap-outline-show-free' checks.
Each group is a list of modifiers, as in `event-convert-list', applied
to the printable characters, SPC, and function keys such as \\`<tab>'
and \\`<f1>', also with shift."
  :type '(repeat (set (const control) (const meta) (const shift)
                      (const super) (const hyper) (const alt))))

(defface keymap-outline-free-key
  '((t :inherit help-key-binding))
  "Face for keys that are free in `keymap-outline-show-free'.")

(defface keymap-outline-used-key
  '((t :inherit shadow))
  "Face for keys that are bound in `keymap-outline-show-free'.")

(defface keymap-outline-partly-used-key
  '((t :inherit warning))
  "Face for keys bound in some buffers in `keymap-outline-show-free'.")

(defface keymap-outline-reserved-key
  '((t :underline t))
  "Face for keys reserved for users, such as \\`C-c a'.
It is added after the face that shows whether the key is free, so
only its attributes that face leaves unset, such as the underline,
take effect.")

(defconst keymap-outline--free-bases
  ;; The function keys that `local-function-key-map' translates to chars
  ;; when they are unbound, such as <tab> to TAB, but not with
  ;; modifiers, such as C-<tab>.
  (let* ((keys '(tab return escape backspace delete))
         (fkeys (mapcar (lambda (n) (intern (format "f%d" n)))
                        (number-sequence 1 12)))
         ;; Shift is folded into chars, such as C-! for C-S-1, but not
         ;; into function keys.  S-<escape> is rarely used.
         (shifted (lambda (keys)
                    (mapcar (lambda (key) (list 'shift key))
                            (remq 'escape keys)))))
    (list (append (number-sequence ?0 ?9)
                  (number-sequence ?a ?z)
                  (number-sequence ?A ?Z))
          ;; The symbols in the order of a custom Dvorak keyboard: the
          ;; shifted digits, the punctuation, and the shifted punctuation.
          (append "!@#$%^&*()',.;/-=[]`\\\"<>:?_+{}~|" '(?\s) keys)
          (funcall shifted keys)
          fkeys
          (funcall shifted fkeys)))
  "Keys that `keymap-outline-free-modifiers' are applied to, by line.
They are in the order `keymap-outline-show-free' shows them.  A key
is an event, or a list of modifiers and a base event, as in
`event-convert-list'.")

(defconst keymap-outline--ignored-commands
  '(self-insert-command ignore ignore-event undefined)
  "Definitions that are not listed.
They still hide the bindings of the same key in lower keymaps.")

(defvar-local keymap-outline--source nil
  "Buffer whose key bindings `keymap-outline-mode' shows.
It can also be a list of major modes, which stands for their live
buffers, or t for all major modes as in `keymap-outline-show-free'.")

(defvar-local keymap-outline--rows-function #'keymap-outline--rows
  "Function that returns the rows to show.
It is called with the list of source buffers.")

(defun keymap-outline--ignored-p (key)
  "Return non-nil if KEY, a vector, matches `keymap-outline-ignore-regexp'."
  (and keymap-outline-ignore-regexp
       (string-match-p keymap-outline-ignore-regexp (key-description key))))

(defun keymap-outline--walk (fn map &optional prefix)
  "Call FN with each key in MAP and its definition, recursively.
FN is called with KEY, a vector of PREFIX followed by the key in MAP,
and the definition that takes effect, which is a keymap for a prefix
key.  Keys matching `keymap-outline-ignore-regexp' are left out.  When
MAP is composed of several keymaps, a binding in one hides the bindings
of the same key in the keymaps after it, as in the active keymaps."
  (let (seen)
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
           (let ((key (vconcat prefix (list event)))
                 ;; `lookup-key' finds the binding that takes effect: it
                 ;; resolves menu items, and merges the prefix keymaps of
                 ;; the same key unless a keymap before binds a command.
                 (def (lookup-key map (vector event))))
             (unless (keymap-outline--ignored-p key)
               (funcall fn key def)
               (when (keymapp def)
                 (keymap-outline--walk fn def key)))))))
     map)))

(defun keymap-outline--bindings (map)
  "Return the command bindings in MAP as a list of (KEY . DEFINITION).
KEY is a `key-description'.  See `keymap-outline--walk'."
  (let (bindings)
    (keymap-outline--walk
     (lambda (key def)
       (unless (or (keymapp def) (null def)
                   (memq def keymap-outline--ignored-commands))
         (push (cons (key-description key) def) bindings)))
     map)
    bindings))

(defun keymap-outline--def-text (def)
  "Return the text of the key binding DEF."
  (cond ((symbolp def)
         (propertize (symbol-name def) 'face 'font-lock-function-name-face))
        ;; A keyboard macro or a key translation.
        ((or (stringp def) (vectorp def))
         (propertize (key-description def) 'face 'help-key-binding))
        ;; Such as `kmacro' objects.
        (t (symbol-name (or (oclosure-type def) 'lambda)))))

(defun keymap-outline--format-def (def)
  "Return the text of the key binding DEF in `keymap-outline-show'."
  (propertize (concat "→ " (keymap-outline--def-text def))
              'keymap-outline-command (and (symbolp def) def)))

(defun keymap-outline--rows (buffers)
  "Return active bindings in the first of BUFFERS as rows.
Each row is (PREFIX KEY LINES), where PREFIX is the list of keys
before the last one in KEY, and LINES is the list of the text of
KEY's binding."
  (with-current-buffer (car buffers)
    (cl-loop for (key . def) in (sort (keymap-outline--bindings
                                       (make-composed-keymap
                                        (append (current-active-maps t)
                                                (list key-translation-map))))
                                      :key #'car)
             collect (list (butlast (split-string key " "))
                           key (list (keymap-outline--format-def def))))))

(defun keymap-outline--key-notes (prefix events)
  "Return the keys of EVENTS after the vector PREFIX, with notes.
Return a list of (KEY . NOTE), where NOTE is nil, or the text of why
KEY is not free in any buffer even if unbound: as in
`read-key-sequence', the quit char quits, ESC is the meta prefix, and
an unbound help event after a prefix lists the prefix's bindings.
Keys that match `keymap-outline-ignore-regexp', or that a key
translation map translates, are not free either."
  (let ((quit (nth 3 (current-input-mode)))
        (tmaps (cl-loop for tmap in (list input-decode-map
                                          local-function-key-map
                                          key-translation-map)
                        for sub = (lookup-key tmap prefix)
                        when (keymapp sub) collect sub)))
    (cl-loop
     for event in events
     for key = (vconcat prefix (list event))
     for trans = (cl-some (lambda (tmap)
                            (let ((trans (lookup-key tmap (vector event))))
                              (and (not (numberp trans)) trans)))
                          tmaps)
     collect (cons key
                   (cond ((eql event quit) "quit")
                         ((eql event meta-prefix-char) "meta prefix")
                         ((and (length> prefix 0)
                               (memq event (cons help-char help-event-list)))
                          "help for the prefix")
                         ((keymap-outline--ignored-p key)
                          "ignored by `keymap-outline-ignore-regexp'")
                         ((keymapp trans) "key translation prefix")
                         (trans (concat "translated to "
                                        (keymap-outline--def-text trans))))))))

(defun keymap-outline--binding-text (pmap event remaps)
  "Return the text of the binding of EVENT in PMAP, or nil if unbound.
PMAP is a keymap or nil.  A command is remapped by REMAPS, the
\\=`[remap]' keymap of the active keymaps, or nil, and `undefined'
counts as unbound."
  (let ((def (and (keymapp pmap) (lookup-key pmap (vector event)))))
    (when (and def (symbolp def) remaps)
      (let ((remapped (lookup-key remaps (vector def))))
        (when (and remapped (not (integerp remapped)))
          (setq def remapped))))
    (cond ((memq def '(nil undefined)) nil)
          ((and (keymapp def) (not (symbolp def))) "prefix key")
          (t (keymap-outline--def-text def)))))

(defun keymap-outline--uses-text (pos &optional object)
  "Return the text of the uses of the key at POS in each buffer group.
OBJECT is the buffer or string of POS, as in `get-text-property'.
The modes with the same use share a line, which counts them instead
of naming them if there are more than three."
  (let* ((uses (get-text-property pos 'keymap-outline-uses object))
         (groups (get-text-property pos 'keymap-outline-modes object))
         (total (length (seq-uniq (apply #'append groups))))
         used free)                     ; used: ((TEXT . MODES) ...), reversed
    (dolist (modes groups)
      (let* ((text (alist-get modes uses nil nil #'eq))
             (entry (and text (assoc text used))))
        (cond (entry (setcdr entry (append (cdr entry) modes)))
              (text (push (cons text modes) used))
              (t (setq free (append free modes))))))
    (string-join
     (cons (propertize (key-description
                        (get-text-property pos 'keymap-outline-key object))
                       'face 'help-key-binding)
           (cl-loop for (text . modes) in (append (nreverse used)
                                                  (and free `(("free" . ,free))))
                    for uniq = (seq-uniq modes)
                    for n = (length uniq)
                    collect (if (<= n 3)
                                (format "%s: %s"
                                        (mapconcat #'symbol-name uniq ", ")
                                        text)
                              (format "%s in %s%d modes" text
                                      (if (= n total) "all " "") n))))
     "\n")))

(defun keymap-outline--help-echo (_window object pos)
  "Return the uses of the key at POS in OBJECT, for `help-echo'."
  (keymap-outline--uses-text pos object))

(defun keymap-outline--reserved-p (key)
  "Return non-nil if KEY, a vector, is reserved for users.
These are \\`C-c' followed by a letter, and \\`<f5>' to \\`<f9>'."
  (pcase key
    (`[?\C-c ,(and c (pred characterp))] (<= ?A (upcase c) ?Z))
    (`[,(or 'f5 'f6 'f7 'f8 'f9)] t)))

(defun keymap-outline--modifier-label (modifiers)
  "Return the `key-description' prefix of MODIFIERS, such as \"C-M-\"."
  ;; A function key, so that no modifier is folded into it, as shift is
  ;; into a char.
  (substring (single-key-description
              (event-convert-list (append modifiers '(f1))))
             0 -4))

(defun keymap-outline--candidates (modifiers)
  "Return the layout of the keys with MODIFIERS for `keymap-outline-show-free'.
It is a list of the lines of `keymap-outline--free-bases', each a list
of (TEXT . EVENT), where TEXT is the key without the modifiers, such
as \"A\" for C-S-a, or \"i\" for C-i, which is TAB."
  (cl-loop for bases in keymap-outline--free-bases
           collect (cl-loop for base in bases
                            for base-list = (ensure-list base)
                            collect (cons (single-key-description
                                           (event-convert-list base-list))
                                          (event-convert-list
                                           (append modifiers base-list))))))

(defun keymap-outline--join (texts)
  "Join TEXTS with a space between them, unless both are one char wide."
  (apply #'concat
         (cl-loop for (text . rest) on texts
                  collect text
                  when (and rest
                            (not (= 1 (string-width text) (string-width (car rest)))))
                  collect " ")))

(defun keymap-outline--key-lines (prefix candidates uses modes)
  "Return the CANDIDATES after the vector PREFIX as a list of lines.
CANDIDATES is from `keymap-outline--candidates'.  USES is a hash
table from the keys that are not free in some buffer groups to a list
of (GROUP-MODES . TEXT), and MODES is the list of the major modes of
each group.  A line whose keys are all used in every group, and none
reserved, is left out."
  (let ((total (length modes)))
    (cl-loop
     for line in candidates
     for shown = nil
     for texts = (cl-loop
                  for (text . event) in line
                  collect
                  (let* ((key (vconcat prefix (list event)))
                         (key-uses (gethash key uses))
                         (used (length key-uses))
                         (reserved (keymap-outline--reserved-p key))
                         (face (cond ((= used 0) 'keymap-outline-free-key)
                                     ((< used total) 'keymap-outline-partly-used-key)
                                     (t 'keymap-outline-used-key))))
                    (when (or (< used total) reserved)
                      (setq shown t))
                    (propertize text
                                'face (if reserved
                                          (list face 'keymap-outline-reserved-key)
                                        face)
                                'keymap-outline-key key
                                'keymap-outline-uses key-uses
                                'keymap-outline-modes modes
                                'help-echo #'keymap-outline--help-echo)))
     when shown
     collect (keymap-outline--join texts))))

(defun keymap-outline--prefixes (map)
  "Return the prefix keys in MAP as vectors, including [].
Prefixes whose keys are all left out, such as the mouse events on
<tab-line>, are left out, but not empty prefix keymaps."
  (let ((prefixes (list (cons [] map)))
        (parents (make-hash-table :test #'equal)))
    (keymap-outline--walk
     (lambda (key def)
       (puthash (substring key 0 -1) t parents)
       ;; Keys after ESC are the meta keys of the keymap before.
       (when (and (keymapp def)
                  (not (eq (aref key (1- (length key))) meta-prefix-char)))
         (push (cons key def) prefixes)))
     map)
    (cl-loop for (prefix . pmap) in prefixes
             when (or (gethash prefix parents)
                      (not (catch 'found
                             (map-keymap (lambda (&rest _) (throw 'found t))
                                         pmap))))
             collect prefix)))

(defun keymap-outline--group-buffers (buffers)
  "Group BUFFERS by their active keymaps, which give the same free keys.
Return a list of (MAP . MODES), where MAP is the composition of the
active keymaps, and MODES are the major modes of the group's buffers."
  (let (groups)                         ; ((MAPS MAP . MODES) ...)
    (dolist (buf buffers)
      (let* ((maps (with-current-buffer buf (current-active-maps t)))
             (mode (keymap-outline--buffer-mode buf))
             (group (cdr (cl-assoc maps groups
                                   :test (lambda (a b) (not (cl-mismatch a b)))))))
        (if group
            (cl-pushnew mode (cdr group))
          (push (list maps (make-composed-keymap maps) mode) groups))))
    (nreverse (mapcar #'cdr groups))))

(defun keymap-outline--free-rows (buffers)
  "Return the keys of BUFFERS by modifier group as rows.
Each row is (PREFIX GROUP LINES), where GROUP names a modifier group
in `keymap-outline-free-modifiers', and LINES is from
`keymap-outline--key-lines'.  The prefixes are those of any of
BUFFERS."
  (let* ((groups (keymap-outline--group-buffers buffers))
         (modes (mapcar #'cdr groups))
         (modifier-groups
          (mapcar (lambda (modifiers)
                    (cons (if modifiers
                              (keymap-outline--modifier-label modifiers)
                            "(none)")
                          (keymap-outline--candidates modifiers)))
                  keymap-outline-free-modifiers))
         (events (delete-dups
                  (cl-loop for (_ . lines) in modifier-groups
                           nconc (mapcan (lambda (line) (mapcar #'cdr line))
                                         lines))))
         (prefixes (delete-dups
                    (mapcan (lambda (group) (keymap-outline--prefixes (car group)))
                            groups)))
         ;; The notes do not depend on the buffer, so compute them once.
         (prefix-notes (mapcar (lambda (prefix)
                                 (cons prefix
                                       (keymap-outline--key-notes prefix events)))
                               prefixes))
         (uses (make-hash-table :test #'equal)))
    (pcase-dolist (`(,map . ,group-modes) groups)
      (let ((remaps (lookup-key map [remap])))
        (pcase-dolist (`(,prefix . ,notes) prefix-notes)
          (let* ((pmap (lookup-key map prefix))
                 ;; A number means the keys before it run a command.
                 (pmap (if (numberp pmap)
                           (lookup-key map (substring prefix 0 pmap))
                         pmap))
                 (blocked (and pmap (not (keymapp pmap)) (not (eq pmap 'undefined))
                               (concat "prefix runs "
                                       (keymap-outline--def-text pmap)))))
            (pcase-dolist (`(,key . ,note) notes)
              (let ((use (or blocked
                             (keymap-outline--binding-text
                              pmap (aref key (1- (length key)))
                              (and (keymapp remaps) remaps))
                             note)))
                (when use
                  (push (cons group-modes use) (gethash key uses)))))))))
    (cl-loop
     for prefix in prefixes
     for heading = (split-string (key-description prefix) " " t)
     nconc (cl-loop
            for (label . candidates) in modifier-groups
            for lines = (keymap-outline--key-lines prefix candidates uses modes)
            when lines
            collect (list heading label lines)))))

(defun keymap-outline--buffer-mode (buf)
  "Return the major mode of buffer BUF."
  (buffer-local-value 'major-mode buf))

(defun keymap-outline--user-buffers ()
  "Return the live buffers whose key bindings can be shown.
Internal buffers, named with a leading space, and the buffers of
`keymap-outline-mode' are left out."
  (cl-remove-if (lambda (buf)
                  (or (string-prefix-p " " (buffer-name buf))
                      (eq (keymap-outline--buffer-mode buf) 'keymap-outline-mode)))
                (buffer-list)))

(defun keymap-outline--source-buffers ()
  "Return the live buffers that `keymap-outline--source' stands for."
  (or (pcase keymap-outline--source
        ((pred bufferp)
         (and (buffer-live-p keymap-outline--source)
              (list keymap-outline--source)))
        ('t
         (cl-remove-if (lambda (buf)
                         (memq (keymap-outline--buffer-mode buf)
                               keymap-outline-ignored-modes))
                       (keymap-outline--user-buffers)))
        (modes
         (cl-remove-if-not (lambda (buf)
                             (memq (keymap-outline--buffer-mode buf) modes))
                           (keymap-outline--user-buffers))))
      (user-error "No source buffer is live")))

(defun keymap-outline--header (buffers)
  "Return the header line naming the source BUFFERS."
  (cl-flet ((buffers (n) (format (ngettext "%d buffer" "%d buffers" n) n)))
    ;; Escape `%' in buffer names from mode line constructs.
    (string-replace
     "%" "%%"
     (concat "Keys in "
             (pcase keymap-outline--source
               ((pred bufferp)
                (format "%s (%s)" (buffer-name (car buffers))
                        (keymap-outline--buffer-mode (car buffers))))
               ('t
                (let ((n (length (seq-uniq (mapcar #'keymap-outline--buffer-mode
                                                   buffers)))))
                  (format (ngettext "all %d major mode (%s)"
                                    "all %d major modes (%s)" n)
                          n (buffers (length buffers)))))
               (modes
                (mapconcat
                 (lambda (mode)
                   (format "%s (%s)" mode
                           (buffers (cl-count mode buffers
                                              :key #'keymap-outline--buffer-mode))))
                 modes ", ")))))))

(defun keymap-outline--revert (&rest _)
  "Render the rows of the buffers of `keymap-outline--source'."
  (let* ((buffers (keymap-outline--source-buffers))
         ;; Sorting by prefix puts each prefix's own rows before its
         ;; sub-prefixes, as outline headings need, and the top level,
         ;; whose prefix is empty, first.  `sort' is stable, so the rows
         ;; of a prefix keep the order of the rows function.
         (rows (mapcar (lambda (row)
                         (if (car row) row (cons '("(top level)") (cdr row))))
                       (sort (funcall keymap-outline--rows-function buffers)
                             :key #'car)))
         (width (apply #'max 0 (mapcar (lambda (r)
                                         (+ (* 2 (length (car r)))
                                            (string-width (cadr r))))
                                       rows)))
         ;; Indent the lines after the first one of a row, and the
         ;; continuation of long lines.
         (pad (make-string (1+ width) ?\s))
         (inhibit-read-only t)
         path)
    (setq header-line-format (keymap-outline--header buffers)
          wrap-prefix pad)
    (erase-buffer)
    (pcase-dolist (`(,prefix ,key ,lines) rows)
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
        (insert (make-string indent ?\s)
                (propertize key 'face 'help-key-binding)
                (make-string (- width indent (string-width key)) ?\s)
                " ")
        (cl-loop for (line . rest) on lines
                 do (insert line (if rest (concat "\n" pad) "\n")))))
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
  "Toggle the heading at point, or describe the key or command at point."
  (interactive)
  (let ((key (get-text-property (point) 'keymap-outline-key))
        ;; The binding ends the line in `keymap-outline-show'.
        (command (get-text-property (max (pos-bol) (1- (pos-eol)))
                                    'keymap-outline-command)))
    (cond ((outline-on-heading-p) (outline-toggle-children))
          ;; A key in `keymap-outline-show-free', bound or not.
          ((and key (bufferp keymap-outline--source))
           (describe-key (list (cons key key))
                         (car (keymap-outline--source-buffers))))
          (key (message "%s" (keymap-outline--uses-text (point))))
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
  "Major mode for `keymap-outline-show' and `keymap-outline-show-free' output.
\\{keymap-outline-mode-map}"
  (buffer-disable-undo)
  (setq-local revert-buffer-function #'keymap-outline--revert
              outline-search-function #'outline-search-level
              outline-level (lambda () (get-text-property (point) 'outline-level))
              outline-minor-mode-cycle t
              outline-minor-mode-use-buttons 'in-margins
              word-wrap t)
  (outline-minor-mode 1))

;;;###autoload
(defun keymap-outline-show ()
  "Show all key bindings active in the current buffer, folded by prefix."
  (interactive)
  (keymap-outline--show "*keymap-outline*" #'keymap-outline--rows
                        (current-buffer)))

(defun keymap-outline--read-modes ()
  "Read major modes of live buffers, or t for all of them by default."
  (let* ((modes (seq-uniq (mapcar #'keymap-outline--buffer-mode
                                  (keymap-outline--user-buffers))))
         ;; "all" is a candidate, so that completion UIs that select the
         ;; first candidate, such as Vertico, can choose it.
         (chosen (completing-read-multiple (format-prompt "Major modes" "all")
                                           (cons "all" (mapcar #'symbol-name modes))
                                           nil t nil nil "all")))
    (if (or (null chosen) (member "all" chosen))
        t
      (mapcar #'intern chosen))))

;;;###autoload
(defun keymap-outline-show-free (&optional modes)
  "Show which keys are free in the current buffer, folded by prefix.
With a prefix argument, read major MODES, and show which keys are
free in all live buffers in them.  MODES t, the default, stands for
all major modes of live buffers, except `keymap-outline-ignored-modes',
and \\`g' collects them again.

Under each prefix key, the keys with each modifier group in
`keymap-outline-free-modifiers' are shown in `keymap-outline-free-key'
face when they are free, in `keymap-outline-used-key' face when they
are bound, and in `keymap-outline-partly-used-key' face when they are
bound in some buffers only.  Keys reserved for users, such as \\`C-c a'
and \\`<f5>', are also shown in `keymap-outline-reserved-key' face.
\\`RET' on a key describes it, or with MODES shows its binding in
each mode, as hovering over it does."
  (interactive (list (and current-prefix-arg (keymap-outline--read-modes))))
  (keymap-outline--show "*keymap-outline-free*" #'keymap-outline--free-rows
                        (or modes (current-buffer))))

(defun keymap-outline--show (name rows-function source)
  "Show the rows of ROWS-FUNCTION for SOURCE in buffer NAME.
SOURCE is as in `keymap-outline--source'."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (keymap-outline-mode)
      (setq keymap-outline--source source
            keymap-outline--rows-function rows-function)
      (revert-buffer))
    (pop-to-buffer buf)))

(provide 'keymap-outline)
;;; keymap-outline.el ends here
