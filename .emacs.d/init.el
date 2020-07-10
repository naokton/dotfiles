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
  :config
  (setq auto-save-default nil)
  (setq delete-auto-save-files t)
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
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
  `((custom-file . ,(expand-file-name "custom.el" user-emacs-directory)))
  :config
  (when (file-exists-p custom-file)
    (load custom-file)))

;;;;----------------------------------------------------------------
;;;; Utilities
;;;;----------------------------------------------------------------
(leaf elscreen
  :ensure t
  :config
  (setq elscreen-prefix-key "\C-o")
  (setq elscreen-tab-display-control nil)
  (setq elscreen-tab-display-kill-screen nil)
  (elscreen-start))

(leaf undo-fu
  :ensure t
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "M-z") 'undo-fu-only-redo)
  (setq undo-limit 600000)
  (setq undo-strong-limit 900000))

(leaf company
  :ensure t
  :hook
  (after-init-hook . global-company-mode)
  :config
  (with-eval-after-load 'company
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-search-map (kbd "C-n") 'company-select-next)
    (define-key company-search-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
    (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)
    (setq company-selection-wrap-around t)))

(leaf ddskk
  :ensure t
  :config
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
  (setq skk-kakutei-when-unique-candidate t))

(leaf migemo
  :ensure t
  :require t
  :setq
  (migemo-command . "cmigemo")
  (migemo-options . '("-q" "--emacs"))
  (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict")
  (migemo-user-dictionary . nil)
  (migemo-regex-dictionary . nil)
  (migemo-coding-system . 'utf-8-unix)
  :config
  (migemo-init))

(leaf lsp-mode
  :ensure t
  :setq
  ;; performance https://emacs-lsp.github.io/lsp-mode/page/performance/
  `(read-process-output-max . ,(* 1024 1024 3)) ;; 1mb
  (gc-cons-threshold . 100000000)
  :hook
  ((yaml-mode-hook sh-mode-hook python-mode-hook) . lsp)
  :config
  (leaf lsp-ui
    :ensure t))

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
  ;; (global-set-key "\C-xf" 'recentf-open-files)
  (recentf-mode 1))

(leaf *ivy-counsel-swiper
  :config
  (leaf ivy
    :ensure t
    :require t
    :config
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

  (leaf counsel
    :ensure t
    :require t
    :config
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "M-y") 'counsel-yank-pop)
    (global-set-key (kbd "C-M-z") 'counsel-fzf)
    (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
    (global-set-key (kbd "C-M-f") 'counsel-rg)
    ;; add --hidden --smart-case options
    (setq counsel-rg-base-command "rg -M 120 --with-filename --no-heading --line-number --hidden --glob !.git --smart-case --color never %s")
    (delete '(counsel-M-x . "^") ivy-initial-inputs-alist)
    (counsel-mode 1))

  (leaf swiper
    :ensure t
    :require t
    :config
    (global-set-key (kbd "M-s") 'swiper)))

(leaf projectile
  :ensure t counsel-projectile
  :require t
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-.") 'projectile-next-project-buffer)
  (define-key projectile-mode-map (kbd "C-,") 'projectile-previous-project-buffer)
  (projectile-mode +1))

(leaf flycheck
  :ensure t
  :hook
  (after-init-hook . global-flycheck-mode)
  :setq
  (flycheck-global-modes . '(shell-script-mode yaml-mode)))

(leaf dumb-jump
  :ensure t
  :setq
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

(leaf macrostep
  :ensure t
  :bind ("C-c e" . macrostep-expand))

(leaf open-junk-file
  :ensure t
  :after open-junk-file
  :setq
  (open-junk-file-format . "~/junk/%Y/%Y%m%d-%H%M%S.org")
  :config
  (global-set-key (kbd "C-x j") 'open-junk-file))

(leaf rainbow-mode :ensure t)

;;;;----------------------------------------------------------------
;;;; Major modes/Language config
;;;;----------------------------------------------------------------
(leaf dired
  :after dired
  :setq
  (dired-listing-switches . "-alh")
  :hook
  (dired-mode-hook . hl-line-mode)
  (dired-mode-hook . (lambda () (display-line-numbers-mode -1)))
  :config
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "C-o") nil)
  (leaf all-the-icons-dired
    :ensure t
    ;; Initial setup: M-x all-the-icons-install-fonts
    :hook dired-mode-hook)
  (leaf dired-sidebar
    :ensure t
    :config
    (add-hook 'dired-sidebar-mode-hook
              (lambda ()
                (unless (file-remote-p default-directory))))
    (global-set-key (kbd "C-x C-n") 'dired-sidebar-toggle-sidebar)))

(leaf org
  :ensure t
  :config
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
</style>"))

(leaf vue-mode
  :ensure t
  :hook
  ;; https://github.com/AdamNiederer/vue-mode/issues/74#issuecomment-577338222
  (vue-mode-hook . (lambda () (setq syntax-ppss-table nil))))

(leaf *javascript
  :config
  (leaf js-mode
    :setq
    (js-indent-level . 2))

  (leaf prettier-js
    :ensure t
    :setq
    (prettier-js-args . '("--trailing-comma" "all"
                          "--bracket-spacing" "false"
                          ))))

(leaf yaml-mode
  :ensure t
  :config
  (leaf highlight-indent-guides
    :ensure t
    :hook
    (yaml-mode-hook . highlight-indent-guides-mode)
    :config
    (eval-after-load 'highlight-indent-guides
      '(progn
         (setq highlight-indent-guides-responsive nil)
         (setq highlight-indent-guides-auto-odd-face-perc 10)
         (setq highlight-indent-guides-auto-even-face-perc 20)
         (setq highlight-indent-guides-method 'fill)))))

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
  :config
  ;; initial size and position
  (setq initial-frame-alist '((top . 100)
                              (left . 600)
                              (width . 160)
                              (height . 85)))
  ;; don't show default something
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (setq inhibit-startup-message t)
  (scroll-bar-mode 0)
  ;; window title
  (setq frame-title-format "%b [%f]"))

(leaf *font-confnig
  :config
  (create-fontset-from-ascii-font "Menlo-12:weight=normal:slant=normal" nil "menlokakugo")
  (set-fontset-font "fontset-menlokakugo"
                    'unicode
                    (font-spec :family "Hiragino Kaku Gothic ProN")
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
  ;; font-size rescale 対策
  (setq face-font-rescale-alist '((".*Hiragino.*" . 1.2) (".*Menlo.*" . 1.0))))

(leaf *theme-config
  :config
  (leaf color-theme-sanityinc-tomorrow
    :ensure t
    :require t
    :config
    (load-theme 'sanityinc-tomorrow-bright t)
    (custom-theme-set-faces
     'sanityinc-tomorrow-bright
     '(cursor ((t (:background "#e7c547"))))
     '(line-number-current-line ((t (:background "#969896" :foreground "#eaeaea" :weight bold))))
     '(mode-line-buffer-id ((t (:foreground "#eaeaea" :weight bold))))))

  (setq mmm-submode-decoration-level 0))

(leaf *appearance-config
  :setq-default
  (indent-tabs-mode . nil)   ; use spaces
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
    :setq
    (doom-modeline-major-mode-color-icon . nil)
    (doom-modeline-vcs-max-length . 24)
    :config
    (doom-modeline-mode 1)
    (line-number-mode 0)
    (column-number-mode 0))

  (leaf nyan-mode
    :ensure t
    :setq
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
    :config
    (global-set-key "\C-h" 'delete-backward-char)
    (define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char))
  (leaf *key-other-window
    :config
    (define-minor-mode overriding-minor-mode
      "強制的にC-tを割り当てる"             ;説明文字列
      t                                     ;デフォルトで有効にする
      ""                                    ;モードラインに表示しない
      `((,(kbd "C-t") . other-window))))
  (leaf *key-buffer
    :config
    (global-set-key (kbd "C-,") 'bs-cycle-previous)
    (global-set-key (kbd "C-.") 'bs-cycle-next))
  (leaf *key-elstreen
    :config
    (global-set-key (kbd "C->") 'elscreen-next)
    (global-set-key (kbd "C-<") 'elscreen-previous))
  (leaf *key-org-mode
    :config
    (add-hook 'org-mode-hook
              (lambda ()
                (define-key org-mode-map (kbd "C-c ,") 'org-insert-structure-template)
                (define-key org-mode-map (kbd "C-,") 'bs-cycle-previous)
		(define-key ac-complete-mode-map [tab] 'ac-expand))))
  (leaf *key-highlight
    :config
    (global-set-key [(control f3)] 'highlight-symbol-at-point)
    (global-set-key [(control shift f3)] 'unhighlight-regexp))
  (leaf *key-view-mode
    :after view-mode
    :config
    (define-key view-mode-map (kbd "h") 'View-scroll-line-forward)
    (define-key view-mode-map (kbd "t") 'View-scroll-line-backward)
    (define-key view-mode-map (kbd "H") 'View-scroll-half-page-forward)
    (define-key view-mode-map (kbd "T") 'View-scroll-half-page-backward)))

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
  :config
  (require 'sequential-command-config)
  (sequential-command-setup-keys))
