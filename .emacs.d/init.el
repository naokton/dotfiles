;;;;----------------------------------------------------------------
;;;; leaf.el setup
;;;;----------------------------------------------------------------
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    ;; :init
    ;; ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    ;; (leaf hydra :ensure t)
    ;; (leaf el-get :ensure t)
    ;; (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))


;;;;----------------------------------------------------------------
;;;; System config
;;;;----------------------------------------------------------------
(leaf *saving
  :setq
  (auto-save-default . nil)
  (delete-auto-save-files . t)
  (make-backup-files . nil)
  (create-lockfiles . nil)
  :config
  (auto-save-visited-mode t))

(leaf *path
  :config
  (add-to-list 'load-path "~/.emacs.d/manual-lisp")
  (leaf exec-path-from-shell
    :ensure t
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

(leaf *custom-export
  ;; custom-set-variables and custom-set-faces
  :setq
  `(custom-file . ,(expand-file-name "custom.el" user-emacs-directory))
  ;; :config
  ;; (when (file-exists-p custom-file)
  ;;   (load custom-file))
  )

;;;;----------------------------------------------------------------
;;;; Utilities
;;;;----------------------------------------------------------------
(leaf elscreen
  :ensure t
  :setq
  (elscreen-prefix-key . "\C-o")
  (elscreen-tab-display-control . nil)
  (elscreen-tab-display-kill-screen . nil)
  :config
  (elscreen-start))

(leaf undo-fu
  :ensure t
  ;; undo-limit/undo-strong-limit should be set when startup. If undo-limit is
  ;; set after load, undo limit remains default until you call undo.
  :leaf-defer nil
  :bind
  ("C-z" . nil)
  ("C-z" . undo-fu-only-undo)
  ("M-z" . undo-fu-only-redo)
  :setq
  ;; These are not undo-fu's variable.
  (undo-limit . 600000)
  (undo-strong-limit . 900000))

(leaf company
  :ensure t
  :hook
  (after-init-hook . global-company-mode)
  :config
  (with-eval-after-load 'company
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq company-selection-wrap-around t)))

(leaf ddskk
  :ensure t
  :leaf-defer nil
  :bind
  ("C-\\" . skk-mode)
  :setq
  (skk-use-act . t)
  (skk-kakutei-when-unique-candidate . t)
  (skk-extra-jisyo-file-list
   . '("~/.emacs.d/skk-get-jisyo/SKK-JISYO.JIS2"
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
       "~/.emacs.d/skk-get-jisyo/SKK-JISYO.zipcode")))

(leaf migemo
  :when (executable-find "cmigemo")
  :ensure t
  :require t
  :custom
  (migemo-command . "cmigemo")
  (migemo-options . '("-q" "--emacs"))
  (migemo-user-dictionary . nil)
  (migemo-regex-dictionary . nil)
  (migemo-coding-system . 'utf-8-unix)
  :config
  (leaf *macos-migemo
    :when (eq system-type "darwin")
    :custom
    (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict"))
  (leaf *linux-cmigemo
    :when (eq system-type "gnu/linux")
    :custom
    (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict"))
  (migemo-init))

(leaf lsp-mode
  :ensure t
  :custom
  (lsp-enable-folding . nil)
  (lsp-enable-snippet . nil)
  (lsp-enable-symbol-highlighting . nil)
  (lsp-enable-links . nil)
  :setq
  ;; performance https://emacs-lsp.github.io/lsp-mode/page/performance/
  `(read-process-output-max . ,(* 3 1024 1024))
  `(gc-cons-threshold . ,(* 100 1024 1024))
  :hook
  (yaml-mode-hook . lsp)
  (sh-mode-hook . lsp)
  (python-mode-hook . lsp)
  (go-mode-hook . lsp)
  (js-mode-hook . lsp))

(leaf lsp-ui
  :ensure t
  :custom
  (lsp-ui-doc-enable . nil)
  (lsp-ui-doc-position . 'top))

(leaf *ediff
  :setq
  (ediff-window-setup-function . 'ediff-setup-windows-plain)
  (ediff-split-window-function . 'split-window-horizontally))

(leaf magit
  :ensure t)

(leaf recentf-ext
  :ensure t
  :require t
  :setq
  (recentf-auto-cleanup . 600)
  (recentf-max-saved-items . 1000)
  (recentf-exclude . '("\\.elc$"
                       "\\.pyc$"
                       "\\.cache$"
                       ))
  :config
  (recentf-mode 1))

(leaf *ivy-counsel-swiper
  :config
  (leaf ivy
    :ensure t
    :custom
    (ivy-truncate-lines . nil)
    (ivy-wrap . t) ;; リスト先頭で `C-p' するとき，リストの最後に移動する
    (ivy-height . 30)
    (ivy-count-format . "(%d/%d) ")
    :config
    ;; ミニバッファでコマンド発行を認める
    (when (setq enable-recursive-minibuffers t)
      (minibuffer-depth-indicate-mode 1)) ;; 何回層入ったかプロンプトに表示．
    (ivy-mode 1))
  (leaf counsel
    :ensure t
    :custom
    ;; add --hidden --smart-case options
    (counsel-rg-base-command
     . "rg -M 120 --with-filename --no-heading --line-number --hidden --glob !.git --smart-case --color never %s")
    :config
    (delete '(counsel-M-x . "^") ivy-initial-inputs-alist)
    (counsel-mode 1))
  (leaf swiper :ensure t))

(leaf projectile
  :ensure t counsel-projectile
  :require t
  :config
  (projectile-mode +1))

(leaf flycheck
  :ensure t
  :hook
  (after-init-hook . global-flycheck-mode)
  :setq
  (flycheck-global-modes . '(shell-script-mode yaml-mode)))

(leaf dumb-jump
  :ensure t
  :custom
  (dumb-jump-selector . 'ivy)
  :config
  (dumb-jump-mode)) ; enable default keybindings

(leaf smart-jump
  :ensure t
  :config
  (smart-jump-setup-default-registers))

(leaf git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(leaf macrostep :ensure t)

(leaf open-junk-file
  :ensure t
  :custom
  (open-junk-file-format . "~/junk/%Y/%Y%m%d-%H%M%S.org"))

(leaf rainbow-mode :ensure t)

;;;;----------------------------------------------------------------
;;;; Major modes/Language config
;;;;----------------------------------------------------------------
(leaf *dired
  :config
  (leaf dired
    :custom
    (dired-listing-switches . "-alh")
    :hook
    (dired-mode-hook . hl-line-mode)
    (dired-mode-hook . (lambda () (display-line-numbers-mode -1))))
  (leaf all-the-icons-dired
    :ensure t
    ;; Initial setup: M-x all-the-icons-install-fonts
    :hook dired-mode-hook)
  (leaf dired-sidebar
    :ensure t))

(leaf org
  :ensure t
  :custom
  (org-startup-truncated . t)
  (org-html-validation-link . nil)
  (org-html-head
   . "<style type=\"text/css\">
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
</style>"))

(leaf vue-mode
  :ensure t
  :hook
  ;; https://github.com/AdamNiederer/vue-mode/issues/74#issuecomment-577338222
  (vue-mode-hook . (lambda () (setq syntax-ppss-table nil))))

(leaf *javascript
  :config
  (leaf js-mode
    :custom
    (js-indent-level . 2))

  (leaf prettier-js
    :ensure t
    :custom
    (prettier-js-args . '("--trailing-comma" "all"
                          "--bracket-spacing" "false"
                          ))))

(leaf yaml-mode
  :ensure t
  :config
  (leaf highlight-indent-guides
    :ensure t
    :custom
    (highlight-indent-guides-responsive . nil)
    (highlight-indent-guides-auto-odd-face-perc . 10)
    (highlight-indent-guides-auto-even-face-perc . 20)
    (highlight-indent-guides-method . 'fill)
    :hook
    (yaml-mode-hook . highlight-indent-guides-mode)))

(leaf *install-language-modes-without-config
  :ensure (go-mode
           csv-mode
           markdown-mode
           dockerfile-mode
           docker-compose-mode
           nginx-mode
           vue-mode
           groovy-mode
           powershell))

;;;;----------------------------------------------------------------
;;;; Appearance
;;;;----------------------------------------------------------------
(leaf *window-config
  :setq
  (inhibit-startup-message . t)
  ;; window title
  (frame-title-format . "%b [%f]")
  ;; initial size and position
  (initial-frame-alist . '((top . 100)
                           (left . 600)
                           (width . 160)
                           (height . 85)))
  :config
  ;; don't show default something
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0))

(leaf *font-confnig
  :setq
  ;; font-size rescale 対策
  (face-font-rescale-alist . '((".*Hiragino.*" . 1.2) (".*Menlo.*" . 1.0)))
  :config
  (create-fontset-from-ascii-font "Menlo-12:weight=normal:slant=normal" nil "menlokakugo")
  (set-fontset-font "fontset-menlokakugo"
                    'unicode
                    (font-spec :family "Hiragino Kaku Gothic ProN")
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo")))

(leaf *theme-config
  :config
  (leaf color-theme-sanityinc-tomorrow
    :if (window-system)
    :ensure t
    :require t
    :config
    (load-theme 'sanityinc-tomorrow-bright t)
    (custom-theme-set-faces
     'sanityinc-tomorrow-bright
     '(cursor ((t (:background "#e7c547"))))
     '(line-number-current-line ((t (:background "#969896" :foreground "#eaeaea" :weight bold))))
     '(mode-line-buffer-id ((t (:foreground "#eaeaea" :weight bold))))))
  (leaf *mmm-mode
    :custom
    (mmm-submode-decoration-level . 0)))

(leaf *appearance-config
  :custom
  (indent-tabs-mode . nil)   ; use spaces
  (tab-width . 4)            ; default is 8
  (fill-column . 80)
  :config
  (show-paren-mode t)
  (global-display-line-numbers-mode)
  (leaf auto-highlight-symbol
    :ensure t
    :require t
    :config
    (global-auto-highlight-symbol-mode t)))

(leaf *modeline-config
  :config
  (leaf doom-modeline
    :ensure t
    :custom
    (doom-modeline-major-mode-color-icon . nil)
    (doom-modeline-vcs-max-length . 24)
    :config
    (doom-modeline-mode 1)
    (line-number-mode 0)
    (column-number-mode 0))

  (leaf nyan-mode
    :ensure t
    :custom
    (nyan-bar-length . 16)
    :config
    (nyan-mode)))

;; (leaf stripe-buffer
;;   :ensure t
;;   :hook
;;   (dired-mode-hook . turn-on-stripe-buffer-mode))

;;;;----------------------------------------------------------------
;;;; Keys
;;;;----------------------------------------------------------------
(leaf *keybindings
  :config
  (leaf *key-delete-with-c-h
    :bind
    ("C-h" . delete-backward-char)
    (isearch-mode-map
     ("C-h" . isearch-delete-char)))
  (leaf *key-other-window
    ;; https://rubikitch.hatenadiary.org/entry/20101126/keymap
    :config
    (define-minor-mode overriding-minor-mode
      "強制的にC-tを割り当てる"             ;説明文字列
      t                                     ;デフォルトで有効にする
      ""                                    ;モードラインに表示しない
      `((,(kbd "C-t") . other-window))))
  (leaf *key-buffer
    :bind
    ("C-," . bs-cycle-previous)
    ("C-." . bs-cycle-next))
  (leaf *key-highlight
    :bind
    ("C-<f3>" . highlight-symbol-at-point)
    ("C-S-<f3>" . unhighlight-regexp))
  (leaf elscreen
    :bind
    ("C->" . elscreen-next)
    ("C-<" . elscreen-previous))
  (leaf org
    :bind
    ("C-c a" . org-agenda)
    (org-mode-map
     ("C-c ," . org-insert-structure-template)
     ("C-c ." . my/org-insert-timestamp-today-inactive)
     ("C-," . bs-cycle-previous))
    :config
    (defun my/org-insert-timestamp-today-inactive ()
      "Insert inactive timestamp of today"
      (interactive)
      (org-insert-time-stamp (current-time) nil t)))
  (leaf view
    :bind
    (view-mode-map
     ("h" . View-scroll-line-forward)
     ("t" . View-scroll-line-backward)
     ("H" . View-scroll-half-page-forward)
     ("T" . View-scroll-half-page-backward)))
  (leaf ivy
    :bind
    (ivy-minibuffer-map
     ("<escape>" . minibuffer-keyboard-quit)))
  (leaf counsel
    :bind
    ("M-x" . counsel-M-x)
    ("M-y" . counsel-yank-pop)
    ("C-M-z" . counsel-fzf)
    ("C-x C-b" . counsel-ibuffer)
    ("C-M-f" . counsel-rg))
  (leaf swiper
    :bind
    ("M-s" . swiper))
  (leaf company
    :bind
    (company-active-map
     ("C-h" . nil)
     ("<tab>" . company-complete-selection)
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous)
     ("C-s" . company-filter-candidates))
    (company-search-map
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous))
    (emacs-lisp-mode-map
     ("C-M-i" . company-complete)))
  (leaf projectile
    :bind
    (projectile-mode-map
     ("M-p" . projectile-command-map)
     ("C-." . projectile-next-project-buffer)
     ("C-," . projectile-previous-project-buffer)))
  (leaf dired
    :bind
    (dired-mode-map
     ("r" . wdired-change-to-wdired-mode)
     ("C-o" . nil)))
  (leaf open-junk-file
    :bind
    ("C-x j" . open-junk-file))
  (leaf macrostep
    :bind
    ("C-c e" . macrostep-expand))
  (leaf dired-sidebar
    :bind
    ("C-x C-n" . dired-sidebar-toggle-sidebar)))

(leaf key-chord
  :ensure t
  :require t
  :setq
  (key-chord-two-keys-delay . 0.10)
  :config
  (key-chord-mode 1)
  (key-chord-define-global "jk" 'kill-this-buffer)
  (key-chord-define-global "bm" 'counsel-ibuffer)
  (key-chord-define-global "fg" 'counsel-recentf)
  ;; (key-chord-define-global "fl" 'ns-toggle-fullscreen)
  (key-chord-define-global "gs" 'magit-status)
  (key-chord-define-global "vw" 'view-mode)
  (key-chord-define-global ".p" 'counsel-projectile)
  (key-chord-define-global "tm" 'transpose-frame)
  (key-chord-define-global "tb" 'rotate-frame-clockwise))

(leaf sequential-command
  ;; Ex. C-a multiple times; cycle beginning-of-line > beginning-of-buffer > return
  :ensure t
  :require sequential-command-config
  :config
  (sequential-command-setup-keys))
