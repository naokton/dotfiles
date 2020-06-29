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
     (define-key dired-mode-map (kbd "C-o") nil)
     (setq dired-listing-switches "-alh")))
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook (lambda () (display-line-numbers-mode -1)))

;;;; all-the-icons-dired (package.el)
;; Initial setup: M-x all-the-icons-install-fonts
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;;;; dired-sidebar (package.el)
(add-hook 'dired-sidebar-mode-hook
          (lambda ()
            (unless (file-remote-p default-directory))))
(global-set-key (kbd "C-x C-n") 'dired-sidebar-toggle-sidebar)

;;;; company-mode (package.el)
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)
  (setq company-selection-wrap-around t))

;;;; projectile (package.el)
(require 'projectile)
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-.") 'projectile-next-project-buffer)
(define-key projectile-mode-map (kbd "C-,") 'projectile-previous-project-buffer)
;; (projectile-mode +1)

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
(setq org-startup-truncated nil)
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
(setq recentf-max-saved-items 1000)
;; (global-set-key "\C-xf" 'recentf-open-files)
(recentf-mode 1)
(setq recentf-exclude
      '("\\.elc$"
        "\\.pyc$"
        "\\.cache$"
        ))

;;;; Ivy/counsel/swiper (install counsel by package.el)
(when (require 'ivy nil t)
  ;; M-o を ivy-hydra-read-action に割り当てる．
  (when (require 'ivy-hydra nil t)
    (setq ivy-read-action-function #'ivy-hydra-read-action))
  ;; `ivy-switch-buffer' (C-x b) のリストに recent files と bookmark を含める．
  (setq ivy-use-virtual-buffers t)
  ;; ミニバッファでコマンド発行を認める
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1)) ;; 何回層入ったかプロンプトに表示．
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (setq ivy-truncate-lines nil)
  (setq ivy-wrap t) ;; リスト先頭で `C-p' するとき，リストの最後に移動する
  (setq ivy-height 30)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1))

(when (require 'counsel nil t)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-M-z") 'counsel-fzf)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-M-f") 'counsel-rg)
  ;; add --hidden --smart-case options
  (setq counsel-rg-base-command "rg -M 120 --with-filename --no-heading --line-number --hidden --glob !.git --smart-case --color never %s")
  (delete '(counsel-M-x . "^") ivy-initial-inputs-alist)
  (counsel-mode 1))

(when (require 'swiper nil t)
  (global-set-key (kbd "M-s") 'swiper))

;;;; helm.el helm-descbinds.el
;; (require 'helm-config)
;; (helm-descbinds-mode)
;; (eval-after-load 'helm
;;   '(progn
;;      (define-key helm-map (kbd "C-h") 'delete-backward-char)
;;      (helm-migemo-mode 1)))

;; (eval-after-load 'helm-files
;;   '(progn
;;      (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)))
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "M-s") 'helm-swoop)
;; ;; (setq helm-exit-idle-delay 0)
;; (setq helm-buffer-max-length 40)

;;;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-global-modes '(shell-script-mode yaml-mode))
;; (eval-after-load 'flycheck
;;   (define-key flycheck-mode-map (kbd "C-c ! !") 'org-time-stamp-inactive))

;;;; vue-mode
;; https://github.com/AdamNiederer/vue-mode/issues/74#issuecomment-577338222
(add-hook 'vue-mode-hook (lambda () (setq syntax-ppss-table nil)))

;;;; js-mode
(setq js-indent-level 2)

;;;; js-prettier (package.el)
(setq prettier-js-args
      '("--trailing-comma" "all"
        "--bracket-spacing" "false"
        ))

;;;; dumb-jump (package.el)
(setq dumb-jump-selector 'ivy)
(dumb-jump-mode) ; enable default keybindings

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
 '(cursor ((t (:background "#e7c547"))))
 '(line-number-current-line ((t (:background "#969896" :foreground "#eaeaea" :weight bold))))
 '(mode-line-buffer-id ((t (:foreground "#eaeaea" :weight bold)))))
(setq mmm-submode-decoration-level 0)

;; Highlight-symbol
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

;; doom-modeline (package.el)
(doom-modeline-mode 1)
(setq doom-modeline-major-mode-color-icon nil)
(setq doom-modeline-vcs-max-length 24)
(line-number-mode 0)
(column-number-mode 0)

;; nyan-mode
(nyan-mode)
(setq nyan-bar-length 16)

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
(global-display-line-numbers-mode)

;;;; git-gutter (package.el)
(global-git-gutter-mode t)

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

;;;; highlight-indent-guides.el (package.el)
(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
(eval-after-load 'highlight-indent-guides
  '(progn
     (setq highlight-indent-guides-responsive nil)
     (setq highlight-indent-guides-auto-odd-face-perc 10)
     (setq highlight-indent-guides-auto-even-face-perc 20)
     (setq highlight-indent-guides-method 'fill)))


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
(key-chord-define-global "bm" 'counsel-ibuffer)
(key-chord-define-global "fg" 'counsel-recentf)
;; (key-chord-define-global "fl" 'ns-toggle-fullscreen)
(key-chord-define-global "gs" 'magit-status)
(key-chord-define-global "vw" 'view-mode)
(key-chord-define-global ".p" 'counsel-projectile)
(key-chord-define-global "tm" 'transpose-frame)
(key-chord-define-global "tb" 'rotate-frame-clockwise)

;;;; Sequential command - C-a/C-e etc. (package.el install)
(require 'sequential-command-config)
(sequential-command-setup-keys)

;; view-mode
(eval-after-load 'view-mode
  '(progn
     (define-key view-mode-map (kbd "h") 'View-scroll-line-forward)
     (define-key view-mode-map (kbd "t") 'View-scroll-line-backward)
     (define-key view-mode-map (kbd "H") 'View-scroll-half-page-forward)
     (define-key view-mode-map (kbd "T") 'View-scroll-half-page-backward)))

;;;; highlight
(global-set-key [(control f3)] 'highlight-symbol-at-point)
