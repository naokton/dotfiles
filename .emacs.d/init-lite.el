;; -*- lexical-binding: t; -*-
;;
;; Lite emacs config.
;; Usage: /path/to/emacs -Q -nw -l /path/to/this/file

;; SKK (load only required packages' autoloads; skip full package-initialize for faster startup)
;; cdb is ddskk's dependency (used for fast dictionary search)
(dolist (pkg '("ddskk" "cdb"))
  (let ((dir (car (file-expand-wildcards (format "~/.emacs.d/elpa/%s-*" pkg)))))
    (when dir
      (add-to-list 'load-path dir)
      (load (expand-file-name (format "%s-autoloads" pkg) dir) t))))
;; Use plain setq (not customize-set-variable) to avoid force-loading skk just to set variables
(setq skk-use-act t)
(setq skk-kakutei-when-unique-candidate t)
(setq skk-extra-jisyo-file-list
      '("~/.emacs.d/skk-get-jisyo/SKK-JISYO.JIS2"
        ("~/.emacs.d/skk-get-jisyo/SKK-JISYO.JIS2004" . euc-jis-2004)
        ("~/.emacs.d/skk-get-jisyo/SKK-JISYO.JIS3_4" . euc-jis-2004)
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.assoc"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.edict"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.fullname"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.geo"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.itaiji"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.jinmei"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.law"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.lisp"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.mazegaki"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.office.zipcode"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.okinawa"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.propernoun"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.pubdic+"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.station"
        "~/.emacs.d/skk-get-jisyo/SKK-JISYO.zipcode"))

;; Adjust saveing behaviours
(customize-set-variable 'auto-save-default nil)
(customize-set-variable 'delete-auto-save-files t)
(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'create-lockfiles nil)

(customize-set-value 'custom-file "/dev/null")

;; Appearance
(menu-bar-mode 0)
(load-theme 'modus-vivendi-deuteranopia t)

;; Editing
(customize-set-variable 'indent-tabs-mode nil) ; use spaces
(customize-set-variable 'tab-width 4)          ; default is 8
(customize-set-variable 'fill-column 80)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

;; Window
(customize-set-variable 'split-window-preferred-direction 'horizontal)

;; Completion
(customize-set-variable 'completions-detailed t)
(customize-set-variable 'completion-auto-help nil) ; suppress *Completions* popup on TAB
(customize-set-variable 'read-extended-command-predicate #'command-completion-default-include-p)
(fido-vertical-mode 1)

;; Dired
(customize-set-variable 'dired-kill-when-opening-new-dired-buffer t)

;; Keybinds
(repeat-mode +1)
(keymap-global-set "C-h" 'delete-backward-char)
(keymap-global-set "C-x C-k" 'kill-current-buffer)
(keymap-global-set "C-x C-b" 'ibuffer)
(keymap-global-set "C-\\" 'skk-mode)
(keymap-global-set "C-," 'bs-cycle-previous)
(keymap-global-set "C-." 'bs-cycle-next)
