;;;;----------------------------------------------------------------
;;;; System setting
;;;;----------------------------------------------------------------
;;;; Emacs default saving

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq auto-save-default nil)
(setq delete-auto-save-files t)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(auto-save-visited-mode t)

;; load path
(add-to-list 'load-path "~/.emacs.d/manual-lisp")

;; exec-path-from-shell (package.el installed)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;;; tab/space
(setq-default indent-tabs-mode nil)

;;;; elscreen (package.el installed)
(setq elscreen-prefix-key "\C-o")
(setq elscreen-tab-display-control nil)
(setq elscreen-tab-display-kill-screen nil)
(elscreen-start)

;;;; undo-fu (package.el)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "M-z") 'undo-fu-only-redo)
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

;;;; dired
(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
     (setq dired-listing-switches "-alh")))

;;;; DDSKK (package.el install)
(global-set-key "\C-\\" 'skk-mode)
(setq skk-use-act t)
(setq skk-extra-jisyo-file-list
      (list "~/.emacs.d/skk-get-jisyo/SKK-JISYO.JIS2"
            '("~/.emacs.d/skk-get-jisyo/SKK-JISYO.JIS2004" . euc-jis-2004)
            '("~/.emacs.d/skk-get-jisyo/SKK-JISYO.JIS3_4" . euc-jis-2004)
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
(setq skk-kakutei-when-unique-candidate t)


;;;; migemo (package.el)
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(migemo-init)

;;;; Org-mode
(add-hook 'org-mode-hook
	  (lambda ()
            ;; (org-bullets-mode 1)
            (define-key org-mode-map (kbd "C-c !") 'org-time-stamp-inactive)
            ;; (define-key flycheck-mode-map (kbd "C-c ! !") 'org-time-stamp-inactive)
	    (setq indent-tabs-mode nil)))
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-html-validation-link nil)
(setq org-html-head
"<style type=\"text/css\">
  body {
    margin: 0 auto;
    max-width: 90%;
  }
  div#table-of-contents {
    border: dashed 3px #AAA;
    margin: 2em;
    padding: 0 1em;
    max-width: 30em;
  }
  div#table-of-contents h2 {
    font-size: 1.3em;
  }
  code, pre {
    margin: 1em 0;
    white-space: pre-wrap;
    word-wrap: break-word;
  }
  li {
    margin: 0.7em 0;
  }
</style>")

;;;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;;; open-junk-file.el (package.el installed)
;; (require 'open-junk-file)
(eval-after-load "open-junk-file"
  '(progn
     (global-set-key (kbd "C-x j") 'open-junk-file)
     (setq open-junk-file-format "~/junk/%Y/%Y%m%d-%H%M%S.org")))

;;;; recentf-ext.el (package.el installed)
(require 'recentf-ext)
(setq recentf-auto-cleanup 600)
(setq recentf-max-saved-items 200)
;; (global-set-key "\C-xf" 'recentf-open-files)
(recentf-mode 1)
(setq recentf-exclude
      '("\\.elc$"
        "\\.pyc$"
        "\\.cache$"
        ))

;;;; helm.el helm-descbinds.el
(require 'helm-config)
(helm-descbinds-mode)
(eval-after-load 'helm
  '(progn
     (define-key helm-map (kbd "C-h") 'delete-backward-char)
     (helm-migemo-mode 1)))

(eval-after-load 'helm-files
  '(progn
     (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-s") 'helm-swoop)
;; (setq helm-exit-idle-delay 0)
(setq helm-buffer-max-length 40)




;;;; custom-set-variables and custom-set-faces
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;----------------------------------------------------------------
;;;; Appearance
;;;;----------------------------------------------------------------
;;;; Window position & size
(setq initial-frame-alist '((top . 100)
			    (left . 600)
			    (width . 120)
			    (height . 65)))

;;;; Fonts
(create-fontset-from-ascii-font "Menlo-12:weight=normal:slant=normal" nil "menlokakugo")
(set-fontset-font "fontset-menlokakugo"
                  'unicode
                  (font-spec :family "Hiragino Kaku Gothic ProN")
                  nil
                  'append)
(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
;; font-size rescale 対策
(setq face-font-rescale-alist '((".*Hiragino.*" . 1.2) (".*Menlo.*" . 1.0)))
;; デフォルトフェイスにフォントセットを設定
;; 起動時に default-frame-alist に従ったフレームが作成されない現象への対処
;; (set-face-font 'default "fontset-menlokakugo")

;; Theme
;; (load-theme 'tango-dark)
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-bright t)
(custom-theme-set-faces
 'sanityinc-tomorrow-bright
 ;; Set font color from purple to normal foreground color
 '(mode-line-buffer-id ((t (:foreground "#eaeaea" :weight bold)))))
(setq mmm-submode-decoration-level 0)

;; Highlight current line
(global-hl-line-mode)

;; Highlight-symbol
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

;; nyan-mode
(nyan-mode)

;; don't show default something
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq inhibit-startup-message t)
(scroll-bar-mode 0)

;; window title
(setq frame-title-format "%b [%f]")

;; show parenthesis
(show-paren-mode t)

;; fill width
(setq-default fill-column 80)

;; line number
(line-number-mode t)
(global-linum-mode t)

;; truncate default
(setq-default truncate-lines t)

;;;; stripe-buffer.el (package.el installeld)
;; (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)

;;;; view-mode (package.el installed)
(require 'view)
(require 'viewer)
(viewer-stay-in-setup)
(setq viewer-modeline-color-unwritable "tomato"
      viewer-modeline-color-view "orange"
      viewer-modeline-color-default "#888a85") ;same color to comment face
(viewer-change-modeline-color-setup)
(setq view-read-only nil)
;; (setq view-mode-by-default-regexp ".\\(\\.ps\\|\\.gp\\|\\.pl\\|\\.csv\\|\\.el\\|\\.gz\\)$")
;; (setq view-mode-by-default-regexp ".\\..")
;; (setq view-mode-by-default-regexp ".")


;;;;----------------------------------------------------------------
;;;; Keys
;;;;----------------------------------------------------------------
;; Delete with C-h (use F1 for help)
(global-set-key "\C-h" 'delete-backward-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;; Easy other-window
(define-minor-mode overriding-minor-mode
  "強制的にC-tを割り当てる"             ;説明文字列
  t                                     ;デフォルトで有効にする
  ""                                    ;モードラインに表示しない
  `((,(kbd "C-t") . other-window)))

;; buffer movement
(global-set-key (kbd "C-,") 'bs-cycle-previous)
(global-set-key (kbd "C-.") 'bs-cycle-next)

;; elscreen movement
(global-set-key (kbd "C->") 'elscreen-next)
(global-set-key (kbd "C-<") 'elscreen-previous)

;; overwrite keybindings in org-mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map (kbd "C-c ,") 'org-insert-structure-template)
	    (define-key org-mode-map (kbd "C-,") 'bs-cycle-previous)
		(define-key ac-complete-mode-map [tab] 'ac-expand)))
;; key-chord.el
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.10)
(key-chord-define-global "jk" 'kill-this-buffer)
(key-chord-define-global "bm" 'helm-buffers-list)
(key-chord-define-global "fg" 'helm-recentf)
;; (key-chord-define-global "fl" 'ns-toggle-fullscreen)
(key-chord-define-global "gs" 'magit-status)
(key-chord-define-global "vm" 'view-mode)

;;;; Sequential command - C-a/C-e etc. (package.el install)
(require 'sequential-command-config)
(sequential-command-setup-keys)

;; view-mode + viewer?
(define-key view-mode-map (kbd "h") 'View-scroll-line-forward)
(define-key view-mode-map (kbd "t") 'View-scroll-line-backward)
(define-key view-mode-map (kbd "H") 'View-scroll-half-page-forward)
(define-key view-mode-map (kbd "T") 'View-scroll-half-page-backward)

;;;; highlight
(global-set-key [(control f3)] 'higqhlight-symbol-at-point)
