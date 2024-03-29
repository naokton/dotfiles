;;;;----------------------------------------------------------------
;;;; leaf.el setup
;;;;----------------------------------------------------------------
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
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
(leaf tab-bar
  :custom
  (tab-bar-new-tab-choice . "*scratch*")
  (tab-bar-tab-name-function . #'my/project-name-or-default)
  (tab-bar-new-button-show . nil)
  (tab-bar-close-button-show . nil)
  :custom-face
  (tab-bar . '((t (:height 1.0))))
  (tab-bar-tab . '((t (:weight bold :box (:line-width (0 . 2))))))
  (tab-bar-tab-inactive . '((t (:weight normal :box nil))))
  :config
  (tab-bar-mode)
  (defun my/project-name-or-default ()
    "Return project name in project, or default tab-bar tab name"
    (let ((project-name (projectile-project-name)))
      (if (string= "-" project-name)
          (tab-bar-tab-name-current)
        (projectile-project-name))))
  )

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
  ;; requirements: brew install cmigemo
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
    :when (eq system-type 'darwin)
    :custom
    (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict"))
  (leaf *linux-cmigemo
    :when (eq system-type 'gnu/linux)
    :custom
    (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict"))
  (migemo-init))

(leaf vterm
  ;; requirements: brew install cmake libvterm libtool
  :ensure t
  :hook
  (vterm-mode-hook . (lambda () (display-line-numbers-mode -1)))
  :custom
  (vterm-max-scrollback . 10000)
  (vterm-buffer-name-string . "vterm: %s")
  ;; delete "C-h", "C-u", add <f1> and <f2>
  (vterm-keymap-exceptions
   . '("<f1>" "<f2>" "C-x" "C-c" "C-g" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y"))
  :config
  ;; Workaround of not working counsel-yank-pop
  ;; https://github.com/akermu/emacs-libvterm#counsel-yank-pop-doesnt-work
  (defun my/vterm-counsel-yank-pop-action (orig-fun &rest args)
    (if (equal major-mode 'vterm-mode)
        (let ((inhibit-read-only t)
              (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
                     (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))
  (advice-add 'counsel-yank-pop-action :around #'my/vterm-counsel-yank-pop-action))

(leaf vterm-toggle
  :ensure t
  :custom
  (vterm-toggle-scope . 'project)
  :config
  ;; Show vterm buffer in the window located at bottom
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.4)))
  ;; Above display config affects all vterm command, not only vterm-toggle
  (defun my/vterm-new-buffer-in-current-window()
    (interactive)
    (let ((display-buffer-alist nil))
            (vterm)))
  )

(leaf lsp-mode
  :ensure t
  :custom
  (lsp-enable-folding . t)
  (lsp-enable-snippet . nil)
  (lsp-enable-symbol-highlighting . nil)
  (lsp-enable-links . nil)
  (lsp-pylsp-plugins-pydocstyle-enabled . nil)
  (lsp-clients-typescript-log-verbosity . "off")
  (lsp-vetur-validation-template . nil)
  (lsp-vetur-format-enable . nil)
  (lsp-keep-workspace-alive . nil)
  :setq
  ;; performance https://emacs-lsp.github.io/lsp-mode/page/performance/
  `(read-process-output-max . ,(* 3 1024 1024))
  `(gc-cons-threshold . ,(* 100 1024 1024))
  :config
  (add-to-list 'lsp-language-id-configuration '(docker-compose-mode . "yaml"))
  :hook
  (yaml-mode-hook . lsp)                ; npm install -g yaml-language-server
  (sh-mode-hook . lsp)                  ; npm i -g bash-language-server
  (python-mode-hook . lsp)              ; pipx install 'python-lsp-server[all]'; pipx inject python-lsp-server python-lsp-isort
  (js-mode-hook . lsp)                  ; npm i -g typescript-language-server; npm i -g typescript
  (vue-mode-hook . lsp)                 ; npm i -g vls
  (go-mode-hook . lsp-deferred))        ; go get golang.org/x/tools/gopls@latest

(leaf lsp-ui
  :ensure t
  :custom
  (lsp-ui-doc-enable . nil)
  (lsp-ui-sideline-show-code-actions . nil)
  (lsp-ui-peek-peek-height . 50))

(leaf lsp-ivy
  :ensure t)

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

(leaf savehist
  :setq
  (savehist-mode . t))

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
    (ivy-mode 1)
    (add-to-list 'ivy-more-chars-alist '(counsel-rg . 2))) ;; 検索開始する最小文字数
  (leaf counsel
    :ensure t
    :custom
    (counsel-rg-base-command
     . `("rg"
         "--max-columns" "240"
         "--with-filename"
         "--no-heading"
         "--line-number"
         "--color" "never"
         "--smart-case"                 ;added
         "--sort" "path"                ;added
         "%s"))
    ;; '|| true' => partial rg error workaround https://github.com/hlissner/doom-emacs/issues/3038#issuecomment-624165004
    ;; . "rg -M 240 --with-filename --no-heading --line-number --smart-case --sort path --color never %s || true")
    :config
    (delete '(counsel-M-x . "^") ivy-initial-inputs-alist)
    (counsel-mode 1))
  (leaf swiper :ensure t)
  (leaf ivy-migemo :ensure t)
  (leaf ivy-prescient
    :after counsel swiper
    :ensure t
    :custom
    (ivy-prescient-enable-filtering . t)
    (ivy-prescient-enable-sorting . t)
    :config
    (ivy-prescient-mode t)
    (prescient-persist-mode t)
    ;; Workaround regex build error: https://github.com/radian-software/prescient.el/issues/43
    (setf (alist-get 'counsel-rg ivy-re-builders-alist) #'ivy--regex-plus)
    ;; add counsel-rg to ignore list to make '--sort path' option works
    (add-to-list 'ivy-prescient-sort-commands 'counsel-rg t)))

(leaf projectile
  :ensure t counsel-projectile
  :require t
  :config
  (projectile-mode +1)
  :defer-config
  (customize-set-variable 'projectile-globally-ignored-modes
                          (let ((newlist projectile-globally-ignored-modes))
                            ;; (add-to-list 'newlist "fundamental-mode")
                            ;; (add-to-list 'newlist "ibuffer-mode")
                            ;; (add-to-list 'newlist "dired-sidebar-mode")
                            (add-to-list 'newlist "vterm-mode"))))

(leaf copilot
  :url "https://github.com/zerolfx/copilot.el"
  :req "dash" "s" "editorconfig"
  :config
  (add-to-list 'load-path "~/.emacs.d/manual-lisp/copilot.el")
  (require 'copilot)
  (add-hook 'prog-mode-hook 'copilot-mode)
)

(leaf which-key
  :ensure t
  :hook
  (after-init-hook . which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom))

(leaf flycheck
  :ensure t
  :hook
  (after-init-hook . global-flycheck-mode)
  :setq
  (flycheck-global-modes . '(shell-script-mode yaml-mode)))

(leaf imenu-list
  :ensure t
  :bind (("s-i" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation . t)
  :config
  (leaf leaf-tree
    :diminish leaf-tree
    :ensure t))

(leaf dumb-jump
  :ensure t
  :custom
  (dumb-jump-selector . 'ivy)
  :config
  (dumb-jump-mode)) ; enable default keybindings

(leaf smart-jump
  :ensure t
  :custom
  (smart-jump-default-mode-list . '(cc-mode ;; `java-mode', `c-mode', `c++-mode', `objc-mode'
                                    csharp-mode
                                    clojure-mode
                                    elisp-mode
                                    elixir-mode
                                    ;; go-mode
                                    lisp-mode
                                    lispy
                                    python
                                    ruby-mode
                                    rust-mode
                                    scheme
                                    typescript-mode))
  :config
  (smart-jump-setup-default-registers))

(leaf git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(leaf wgrep
  :ensure t)

(leaf origami
  :ensure t lsp-origami
  :hook
  (lsp-after-open-hook . lsp-origami-try-enable))

;; https://stackoverflow.com/a/62502758
;; When called this automatically detects the submode at the current location.
;; It will then either forward to end of tag(HTML) or end of code block(JS/CSS).
;; This will be passed to hs-minor-mode to properly navigate and fold the code.
(defun mhtml-forward (arg)
  (interactive "P")
  (pcase (get-text-property (point) `mhtml-submode)
    (`nil (sgml-skip-tag-forward 1))
    (submode (forward-sexp))))

;; Adds the tag and curly-brace detection to hs-minor-mode for mhtml.
(add-to-list 'hs-special-modes-alist
             '(mhtml-mode
               "{\\|<[^/>]+?"
               "}\\|</[^/>]*[^/]>"
               "<!--"
               mhtml-forward
               nil))

(leaf browse-at-remote
  :ensure t
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected . nil))

(leaf macrostep :ensure t)

(leaf open-junk-file
  :ensure t
  :custom
  (open-junk-file-format . "~/junk/%Y/%Y%m%d-%H%M%S.org"))

(leaf rainbow-mode :ensure t)

(leaf electric-pair-mode
  :config
  (electric-pair-mode 1))

(leaf symbol-overlay :ensure t)

(leaf package
  :custom
  (package-native-compile . t))

(leaf comp
  :custom
  (native-comp-async-report-warnings-errors . nil))

(leaf wdired :ensure t)

(leaf restclient :ensure t)

(leaf gptel
  :ensure t
  :custom
  (gptel-api-key . #'my/retrieve-openapi-token)
  (gptel-default-mode . 'org-mode)
  (gptel-model . "gpt-4")
  :config
  (defun my/retrieve-password-from-keychain (service account)
    "Retrieve password from macOS Keychain."
    (interactive "sService: \nsAccount: ")
    (let ((command (format "security find-generic-password -s '%s' -a '%s' -w" service account))
          (password nil))
      (setq password (string-trim-right (shell-command-to-string command)))
      password))
  (defun my/retrieve-openapi-token ()
    (my/retrieve-password-from-keychain "OpenAI API Key" "local")))

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
  (org-startup-truncated . nil)
  (org-startup-indented . t)
  (org-indent-mode-turns-on-hiding-stars . nil)
  (org-startup-with-inline-images . t)
  (org-src-preserve-indentation . t)
  (org-image-actual-width . nil)
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
  (vue-mode-hook . (lambda () (setq syntax-ppss-table nil)))
  (vue-mode-hook . prettier-mode))

(leaf *javascript
  :config
  (leaf js-mode
    :custom
    (js-indent-level . 2))
  ;; (leaf js2-mode
  ;;   :ensure t
  ;;   :mode "\\.js\\'"
  ;;   :custom
  ;;   (js-indent-level . 2))
  (leaf prettier
    :ensure t
    :hook
    (js-mode-hook . prettier-mode)))

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

(leaf markdown-mode
  :ensure t
  :setq
  (markdown-fontify-code-blocks-natively . t))

(leaf *install-language-modes-without-config
  :ensure (go-mode
           csv-mode
           dockerfile-mode
           docker-compose-mode
           nginx-mode
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
                           (height . 80)))
  :config
  ;; don't show default something
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0))

(leaf *font-confnig
  :setq
  ;; Set fontsize here. rescale doesn't work if size is specified in fontspec
  ;; (face-font-rescale-alist . '((".*Cica.*" . 1.6)))
  :config
  (create-fontset-from-ascii-font "Cica-16" nil "mydefault")
  (set-fontset-font "fontset-mydefault" 'unicode "Hiragino Sans" nil 'append)
  (set-fontset-font "fontset-mydefault" 'symbol "Apple Color Emoji" nil 'append)
  (set-fontset-font "fontset-mydefault" 'emoji "Apple Color Emoji" nil 'append)
  (add-to-list 'default-frame-alist '(font . "fontset-mydefault")))

(leaf *theme-config
  :config
  (leaf color-theme-sanityinc-tomorrow
    :if (window-system)
    :ensure t
    :require t
    :setq
    ;; for emacs 27 (https://emacs.stackexchange.com/questions/48365/#answer-52804)
    (custom--inhibit-theme-enable . nil)
    :config
    (load-theme 'sanityinc-tomorrow-bright t)
    (color-theme-sanityinc-tomorrow--with-colors
     'bright
     (custom-theme-set-faces
      'sanityinc-tomorrow-bright
      `(cursor ((t . (:background ,yellow))))
      `(line-number-current-line ((t . (:background ,comment :foreground ,foreground :weight bold))))
      `(mode-line ((t . (:foreground ,foreground :background ,contrast-bg :weight normal
                                     :box (:line-width 1 :color ,comment)))))
      `(mode-line-inactive ((t . (:inherit mode-line
                                    :foreground ,comment
                                    :background ,highlight
                                    :weight normal
                                    :box (:line-width 1 :color ,contrast-bg)))))
      `(mode-line-buffer-id ((t . (:foreground ,foreground :weight bold)))))))
  (leaf *mmm-mode
    :custom
    (mmm-submode-decoration-level . 0)))

(leaf *appearance-config
  :custom
  (indent-tabs-mode . nil)   ; use spaces
  (tab-width . 4)            ; default is 8
  (fill-column . 100)
  :config
  (show-paren-mode t)
  (global-display-line-numbers-mode)
  (leaf auto-highlight-symbol
    :ensure t
    :require t
    :hook go-mode-hook
    :config
    (global-auto-highlight-symbol-mode t)))

(leaf *modeline-config
  :config
  (leaf doom-modeline
    :ensure t
    ;; Initial setup: M-x nerd-icons-install-fonts
    :custom
    (doom-modeline-major-mode-color-icon . nil)
    (doom-modeline-vcs-max-length . 24)
    (doom-modeline-enable-word-count . t)
    (doom-modeline-buffer-encoding, nondefault)
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
  (leaf *misc
    :bind
    ("M-h" . backward-kill-word))
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
  (leaf symbol-overlay
    :bind
    ("C-<f3>" . symbol-overlay-put)
    ("C-S-<f3>" . symbol-overlay-remove-all))
  (leaf tab-bar
    :bind
    ("C->" . tab-next)
    ("C-<" . tab-previous))
  (leaf origami
    :bind
    (origami-mode-map
     ("C-c f a" . origami-toggle-all-nodes)
     ("C-c f f" . origami-recursively-toggle-node)))
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
  (leaf ivy-migemo
    :bind
    (:ivy-minibuffer-map
     :package ivy
     ("M-m" . ivy-migemo-toggle-migemo)))
  (leaf swiper
    :bind
    ("M-s" . swiper))
  (leaf lsp-ui
    :bind
    (:lsp-ui-mode-map
     ("M-." . lsp-ui-peek-find-definitions)
     ("M-'" . lsp-ui-peek-find-references)))
  (leaf lsp-ivy
    :bind
    (:lsp-mode-map
     ("M-S" . lsp-ivy-workspace-symbol)))
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
  (leaf vterm
    :bind
    ("<f2>" . vterm-toggle)
    (vterm-mode-map
     ("C-<f2>" . my/vterm-new-buffer-in-current-window)
     ("C-<return>" . vterm-toggle-insert-cd)
     ([remap projectile-previous-project-buffer] . vterm-toggle-forward)
     ([remap projectile-next-project-buffer] . vterm-toggle-backward)))
  (leaf copilot
    :bind
    (copilot-completion-map
     ("<tab>" . 'copilot-accept-completion)))
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
  (key-chord-two-keys-delay . 0.20)
  :config
  (key-chord-mode 1)
  (key-chord-define-global "jk" 'kill-this-buffer)
  (key-chord-define-global "bm" 'counsel-ibuffer)
  (key-chord-define-global "fg" 'counsel-recentf)
  ;; (key-chord-define-global "fl" 'ns-toggle-fullscreen)
  (key-chord-define-global "gs" 'magit-status)
  (key-chord-define-global "vw" 'view-mode)
  (key-chord-define-global ".p" 'projectile-find-file-dwim)
  (key-chord-define-global "tm" 'transpose-frame)
  (key-chord-define-global "tb" 'rotate-frame-clockwise))

(leaf sequential-command
  ;; Ex. C-a multiple times; cycle beginning-of-line > beginning-of-buffer > return
  :ensure t
  :config
  (require 'sequential-command)
  (define-sequential-command seq-home
    beginning-of-line beginning-of-buffer seq-return)
  (define-sequential-command seq-end
    end-of-line end-of-buffer seq-return)
  (global-set-key "\C-a" 'seq-home)
  (global-set-key "\C-e" 'seq-end))
