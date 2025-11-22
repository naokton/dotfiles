;;;;----------------------------------------------------------------
;;;; leaf.el setup
;;;;----------------------------------------------------------------
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (customize-set-variable 'package-native-compile t)
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :config
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
  (auto-save-visited-mode t) ; Enable auto save (not auto-save-mode)
  )

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
  :config
  (when (file-exists-p custom-file)
    (load custom-file))
  )

(leaf *read-only-vendor-files
  :doc "Open builtin and installed packages in read-only mode."
  :init
  (defun my/enable-read-only-for-vendor-files ()
    (let ((file (or buffer-file-name "")))
      (when (or
             (string-prefix-p (expand-file-name "~/.emacs.d/elpa/") (expand-file-name file))
             (string-match-p
              "/Emacs.app/Contents/Resources/" (expand-file-name file))
             (string-match-p
              "/.venv/lib/python" (expand-file-name file)))
        (read-only-mode 1))))
  :hook
  (find-file-hook . my/enable-read-only-for-vendor-files))

(leaf *lsp-use-plists
  :config
  (setenv "LSP_USE_PLISTS" "true"))

;;;;----------------------------------------------------------------
;;;; Utilities
;;;;----------------------------------------------------------------
(leaf autorevert
  :global-minor-mode global-auto-revert-mode)

(leaf delsel
  :global-minor-mode delete-selection-mode)

(leaf simple
  :custom
  (kill-whole-line . t))

(leaf ibuffer
  :hook
  (ibuffer-mode-hook . hl-line-mode))

(leaf tab-bar
  :custom
  (tab-bar-new-tab-choice . "*scratch*")
  (tab-bar-tab-name-function . #'my/project-name-or-default)
  (tab-bar-new-button-show . nil)
  (tab-bar-close-button-show . nil)
  ;; Separate tabs evenly. Without this, the last tab occupies whole right space.
  (tab-bar-auto-width-max . nil)
  ;; Add menu button and remove separater.
  (tab-bar-format . '(tab-bar-format-menu-bar tab-bar-format-tabs))
  :config
  (tab-bar-mode)
  (defun my/project-name-or-default ()
    "Return project name when in project, or default tab-bar tab name"
    (let ((project-name (projectile-project-name)))
      (if (string= "-" project-name)
          (tab-bar-tab-name-current)
        (projectile-project-name))))
  )

(leaf package
  :hook
  (package-menu-mode-hook . hl-line-mode))

(leaf vertico
  ;; Minibuffer Completion UI
  :ensure t
  :custom
  (vertico-count . 30)
  (vertico-cycle . t)
  ;; Change from default vertico-sort-history-length-alpha (remove length).
  ;; This function is only used if completion command (completion table) doesn’t specify its own
  ;; display-sort-function.
  (vertico-sort-function . 'vertico-sort-history-alpha)
  ;; Exclude unrelated commands for the current mode from M-x list. 
  ;; This setting also affect outside of vertico.
  (read-extended-command-predicate . #'command-completion-default-include-p)
  ;; Allow recursive command in minibuffers
  (enable-recursive-minibuffers . t)
  ;; (read-extended-command-predicate . nil)
  :init
  (vertico-mode)
  )

(leaf savehist
  :init (savehist-mode))

(leaf orderless
  :ensure t
  :init
  (defun my/orderless-consult-dispatch (word _index _total)
    "Dispatch orderless components that use end anchors, compatible with Consult's tofu.

Recognizes trailing \"$\" (end-of-string) and trailing \"\\_>\" (symbol-end).
Returns a pair (orderless-regexp . REGEXP) that appends `consult--tofu-regexp' so
the anchor still matches when Consult adds invisible disambiguation characters.

ref: URL `https://github.com/minad/consult/wiki#minads-orderless-configuration'"
    (cond
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) consult--tofu-regexp "*\\'")))
     ((string-suffix-p "\_>" word)
      `(orderless-regexp . ,(concat (substring word 0 -3) consult--tofu-regexp "*\\_>")))))
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion))))
  (orderless-matching-styles . '(orderless-literal orderless-regexp))
  (orderless-style-dispatchers . '(my/orderless-consult-dispatch
                                   orderless-affix-dispatch)))

(leaf consult
  :ensure t
  :custom
  (consult-ripgrep-args . "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
                           --smart-case --no-heading --with-filename --line-number --search-zip --sort=path") ; add --sort=path
  (consult-buffer-filter . '("\\` "
                             "\\`:~"
                             "\\`\\*Messages\\*"
                             "\\`\\*Help\\*"
                             "\\`\\*Ibuffer\\*"
                             "\\`\\*Buffer List\\*"
                             "\\`\\*Calendar\\*"
                             "\\`\\*Backtrace\\*"
                             "\\`\\*Disabled Command\\*"
                             "\\`\\*lsp"
                             "\\`\\*pyright"
                             "\\`\\*ruff"
                             "\\`\\*yamlls"
                             "\\`\\*gopls"
                             "\\`\\*bash-ls"
                             "\\`\\*vue-semantic-server"
                             "\\`\\*ts-ls"
                             "\\`\\*magit"
                             "\\`\\*prettier"
                             "\\`\\*Embark"
                             "\\`\\*Org-journal"
                             "\\`\\*diff-hl"
                             "\\`\\*Ediff"
                             "\\`\\*Diff"
                             "\\`\\*Ilist\\*"
                             "\\`\\*gptel-diff"
                             "\\`\\*copilot events"
                             "\\`\\*copilot-language-server-log\\*"
                             "\\`\\*[Cc]opilot-chat-"
                             "\\`\\*Async-native-compile-log\\*"
                             "\\`\\*Completions\\*"
                             "\\`\\*Multiple Choice Help\\*"
                             "\\`\\*Flymake log\\*"
                             "\\`\\*Semantic SymRef\\*"
                             "\\`\\*vc\\*"
                             "\\`newsrc-dribble" ;; Gnus
                             "\\`\\*tramp/.*\\*"))
  :config
  (leaf consult-flycheck :ensure t)
  (leaf consult-lsp :ensure t)
  (leaf xref
    :custom
    (xref-prompt-for-identifier . nil)
    (xref-show-xrefs-function . #'consult-xref)
    (xref-show-definitions-function . #'consult-xref))
  (defun my/consult-line-symbol-at-point ()
    (interactive)
    (let ((sym (thing-at-point 'symbol t)))
      (consult-line (when sym (concat "\\_<" sym "\\_>")))))
  (defun my/consult-ripgrep-symbol-at-point (&optional dir)
    (interactive)
    (consult-ripgrep dir (thing-at-point 'symbol)))
  :defer-config
  (consult-customize
   consult-recent-file consult-ripgrep consult-xref
   :preview-key '(:debounce 0.5 any)))

(leaf marginalia
  :ensure t
  :init
  (marginalia-mode)
  )

(leaf corfu
  :ensure t
  :custom
  (corfu-auto . t)
  (corfu-auto-delay . 0.1)
  (corfu-auto-prefix . 1)
  (corfu-cycle . t)
  :init
  (global-corfu-mode))

(leaf kind-icon
  ;; Add icon to corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf embark
  :ensure t
  :custom
  (embark-help-key . "?")
  (embark-indicators . '(embark-minimal-indicator ; do not pop up key bindisgs buffer
                         embark-highlight-indicator
                         embark-isearch-highlight-indicator)))

(leaf embark-consult
  :ensure t)

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

;; (leaf migemo
;;   ;; requirements: brew install cmigemo
;;   :when (executable-find "cmigemo")
;;   :ensure t
;;   :require t
;;   :custom
;;   (migemo-command . "cmigemo")
;;   (migemo-options . '("-q" "--emacs"))
;;   (migemo-user-dictionary . nil)
;;   (migemo-regex-dictionary . nil)
;;   (migemo-coding-system . 'utf-8-unix)
;;   :config
;;   (leaf *macos-migemo
;;     :when (eq system-type 'darwin)
;;     :custom
;;     (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict"))
;;   (leaf *linux-cmigemo
;;     :when (eq system-type 'gnu/linux)
;;     :custom
;;     (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict"))
;;   (migemo-init))

(leaf vterm
  ;; requirements: brew install cmake libvterm libtool
  :ensure t
  :hook
  (vterm-mode-hook . (lambda () (display-line-numbers-mode -1)))
  :custom
  (vterm-max-scrollback . 100000)
  (vterm-buffer-name-string . "vterm: %s")
  (vterm-always-compile-module . t)
  ;; delete "C-h", "C-u", add <f1> and <f2>
  (vterm-keymap-exceptions
   . '("<f1>" "<f2>" "C-x" "C-c" "C-g" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y"))
  :config
  (defun my/vterm-new-buffer-in-current-window()
    (interactive)
    (let ((display-buffer-alist nil))
      (vterm)))
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                (display-buffer-reuse-window display-buffer-in-side-window)
                (side . bottom)
                (reusable-frames . visible)
                (window-height . 0.4))))

(leaf vterm-toggle
  :ensure t
  :custom
  (vterm-toggle-reset-window-configration-after-exit . nil)
  (vterm-toggle-scope . 'project))

(leaf ultra-scroll
  :ensure t
  :setq
  (scroll-conservatively . 101)
  (scroll-margin . 0)
  :config
  (ultra-scroll-mode 1))

(leaf buffer-env
  :ensure t
  :hook
  (hack-local-variables-hook . buffer-env-update)
  (comint-mode-hook . buffer-env-update)
  :custom
  (buffer-env-script-name . '("uv.lock" "Pipfile" ".envrc"))
  (buffer-env-verbose . t)
  :config
  (add-to-list 'buffer-env-command-alist
               '("/Pipfile\\'" . "\
command -v pipenv >/dev/null &&
pipenv --venv >/dev/null 2>&1 &&
pipenv run env -0 2>/dev/null"))
  (add-to-list 'buffer-env-command-alist
               '("/uv\\.lock\\'" . "\
command -v uv >/dev/null &&
uv sync --frozen >/dev/null 2>&1 &&
uv run env -0 2>/dev/null"))
  (require 'buffer-env)
  )

(leaf compilation-mode
  :custom
  (compilation-scroll-output . 'first-error)
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter) ; colorize output
  )

(leaf treesit-auto
  :ensure t
  :require t
  :custom
  (treesit-auto-install . 'prompt)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

(leaf lsp-mode
  :ensure t
  :custom
  (lsp-enable-folding . t)
  (lsp-enable-snippet . nil)
  (lsp-enable-symbol-highlighting . t)
  (lsp-enable-links . nil)
  (lsp-enable-imenu . nil)
  (lsp-pylsp-plugins-pydocstyle-enabled . nil)
  (lsp-go-build-flags . ["-tags=integration"])
  (lsp-clients-typescript-log-verbosity . "off")
  (lsp-keep-workspace-alive . nil)
  ;; Disable completion by company-mode and use corfu
  (lsp-completion-provider . :none)

  :setq
  ;; performance https://emacs-lsp.github.io/lsp-mode/page/performance/
  `(read-process-output-max . ,(* 3 1024 1024))
  `(gc-cons-threshold . ,(* 100 1024 1024))

  :config
  (add-to-list 'lsp-language-id-configuration '(docker-compose-mode . "yaml"))
  (add-to-list 'lsp-disabled-clients '(typescript-mode . vue-semantic-server))
  (add-to-list 'lsp-disabled-clients '(js-ts-mode . vue-semantic-server))
  (add-to-list 'lsp-disabled-clients '(typescript-ts-mode . vue-semantic-server))
  (add-to-list 'lsp-disabled-clients '(typescript-ts-mode . ts-ls))
  (add-to-list 'lsp-disabled-clients '(js-mode . vue-semantic-server))
  (add-to-list 'lsp-disabled-clients '(css-mode . vue-semantic-server))

  ;; Use orderless for completion style
  ;; ref: https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (defun my/deno-or-js-lsp ()
    "Configure LSP mode for Deno if deno.json or deno.jsonc exists in project root."
    (when-let ((project-root (project-root (project-current))))
      (when (or (file-exists-p (expand-file-name "deno.json" project-root))
                (file-exists-p (expand-file-name "deno.jsonc" project-root)))
        (setq-local lsp-enabled-clients '(deno-ls))))
    (lsp))

  (defun my/cleanup-lsp-workspaces ()
    "Remove LSP workspace folders that no longer exist on the filesystem."
    (interactive)
    (let ((folders (lsp-session-folders (lsp-session))))
      (dolist (folder folders)
        (unless (file-directory-p folder)
          (lsp-workspace-folders-remove folder)))))
  (my/cleanup-lsp-workspaces)

  ;; Enable flycheck support for vue-mode
  ;; https://emacs-lsp.github.io/lsp-mode/page/faq/#the-flycheck-does-not-work-in-typescript-html-and-javascript-blocks-in-vue-mode-how-to-fix-that
  (with-eval-after-load 'lsp-mode
    (mapc #'lsp-flycheck-add-mode '(typescript-mode js-mode css-mode vue-html-mode)))

  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (yaml-ts-mode-hook . lsp)             ; npm install -g yaml-language-server
  (sh-ts-mode-hook . lsp)               ; npm i -g bash-language-server
  (bash-ts-mode-hook . lsp)             ; npm i -g bash-language-server
  ;; (python-ts-mode-hook . lsp-deferred)  ; uv tool install 'python-lsp-server'
  ; npm i -g typescript-language-server; npm i -g typescript; brew install deno
  ((js-ts-mode-hook typescript-ts-mode-hook) . my/deno-or-js-lsp)
  (vue-mode-hook . lsp)                 ; npm i -g @vue/language-server; npm i -g typescript
  (go-ts-mode-hook . lsp-deferred)      ; go get golang.org/x/tools/gopls@latest
  )

(leaf lsp-pyright
  :ensure t   ; and uv tool install basedpyright
  :custom
  (lsp-pyright-langserver-command . "basedpyright")
  (lsp-pyright-type-checking-mode . "off")
  ;; workaround for mixed workspaces https://github.com/emacs-lsp/lsp-pyright/issues/66
  (lsp-pyright-multi-root . nil)
  :hook
  (python-ts-mode-hook . (lambda ()
                           (require 'lsp-pyright)
                           (lsp-deferred))))

(leaf lsp-ui
  :ensure t
  :hook
  ;; Prevent rings when hovering mouse over the tab bar. https://github.com/emacs-lsp/lsp-ui/issues/681
  (lsp-after-initialize-hook . (lambda () (local-set-key (kbd "<tab-bar> <mouse-movement>") #'ignore)))
  :custom
  (lsp-ui-doc-position . 'at-point))

(leaf ediff-wind
  :custom
  (ediff-window-setup-function . 'ediff-setup-windows-plain)
  (ediff-split-window-function . 'split-window-horizontally))

(leaf ediff-diff
  :custom
  (ediff-custom-diff-options . "-u"))

(leaf magit
  :ensure t
  :custom
  (magit-diff-refine-hunk . 'all)
  (magit-buffer-name-format . "*%x%M%v: %t%x") ; prefix with "*" for buffer filtering by consult
  (magit-display-buffer-function . 'magit-display-buffer-same-window-except-diff-v1))

(leaf recentf
  :custom
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

(leaf projectile
  :ensure t
  :require t
  :custom
  (consult-project-function . (lambda (_) (projectile-project-root)))
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
  :ensure t
  :req "dash" "s" "editorconfig" "jsonrpc >= 1.0.24"
  :require t
  :hook
  (python-ts-mode-hook . copilot-mode)
  (go-ts-mode-hook . copilot-mode)
  :custom
  (copilot-idle-delay . 0.1)
  )

(leaf copilot-chat
  :ensure t
  :hook
  (git-commit-setup-hook . copilot-chat-insert-commit-message)
  :custom
  (copilot-chat-default-model . "claude-sonnet-4.5")
  (copilot-chat-commit-model . "gpt-4.1"))

(leaf which-key
  :ensure t
  :hook
  (after-init-hook . which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom))

(leaf transpose-frame
  :ensure t)

(leaf flycheck
  :ensure t
  :hook
  (after-init-hook . global-flycheck-mode)
  :custom-face
  (flycheck-info . '((t (:underline nil :inherit success))))
  :setq
  (flycheck-global-modes . '(shell-script-mode yaml-mode)))

(leaf imenu-list
  :ensure t
  :bind (("s-i" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation . t))

(leaf dumb-jump
  :ensure t
  ;; :custom
  ;; (dumb-jump-selector . 'ivy)
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

;; (leaf git-gutter
;;   :ensure t
;;   :config
;;   (global-git-gutter-mode t))

(leaf diff-hl
  :doc "Git-gutter"
  :ensure t
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(leaf hl-todo
  :ensure t
  :hook
  (prog-mode-hook . hl-todo-mode)
  (sgml-mode-hook . hl-todo-mode))

(leaf wgrep
  :ensure t)

(leaf repeat
  :doc "built-in mode"
  :config
  (repeat-mode +1))

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

(leaf rainbow-mode
  :ensure t
  :custom
  ;; prevent color name strings, e.g. red, blue, etc., from rainbowed
  (rainbow-x-colors . nil))

(leaf electric-pair-mode
  :config
  (electric-pair-mode 1))

(leaf symbol-overlay :ensure t)

(leaf comp
  :custom
  (native-comp-async-report-warnings-errors . nil))

(leaf restclient :ensure t)

(leaf gptel
  :ensure t
  :init
  ;; TODO: use keychain via auth-source
  (defun my/retrieve-password-from-keychain (service account)
    "Retrieve password from macOS Keychain."
    (interactive "sService: \nsAccount: ")
    (let ((command (format "security find-generic-password -s '%s' -a '%s' -w" service account))
          (password nil))
      (setq password (string-trim-right (shell-command-to-string command)))
      password))
  (defun my/retrieve-openapi-token ()
    (my/retrieve-password-from-keychain "OpenAI API Key" "local"))
  (defun my/retrieve-claude-token ()
    (my/retrieve-password-from-keychain "Claude API Key" "work"))
  (defvar my/save-gptel--directory "~/Documents/gptel-history/")
  (defun my/save-gptel ()
    "Save current gptel-mode buffer into a directory, `my/save-gptel--directory', with a timestamped
filename if not saved, otherwise save to the current file."
    (interactive)
    (if (buffer-file-name)
        (save-buffer)
      (let* ((mode-to-ext
              '((org-mode . "org")
                (markdown-mode . "md")))
             (file-ext (or (cdr (assoc major-mode mode-to-ext)) "text"))
             (directory my/save-gptel--directory)
             (filename (format "%s.%s" (format-time-string "%Y%m%d-%H%M%S") file-ext))
             (file-path (concat directory filename)))
        (unless (file-exists-p directory)
          (make-directory directory t))
        (write-file file-path)
        (save-buffer))))
  (add-to-list 'gptel-directives '(email . "You are an expert email editor specializing in natural English communication. Your task is to revise email drafts into clear, slightly casual English while maintaining the original message. When provided with an email draft and context, simply return the corrected version without explanations. Focus on:
- Converting unnatural expressions into common native English phrases
- Keeping language simple and concise
- Using a casual yet professional tone
- Maintaining the original message's intent
Provide only the revised email text without comments or explanations."))
  :custom
  (gptel-api-key . #'my/retrieve-openapi-token)
  (gptel-model . 'claude-sonnet-4-20250514)
  (gptel-backend . `,(gptel-make-anthropic "Claude"
                      :stream t
                      :key #'my/retrieve-claude-token))
  (gptel-default-mode . 'org-mode)
  (gptel-prompt-prefix-alist . '((markdown-mode . "## Reqest\n")
                                 (org-mode . "** Reqest\n")
                                 (text-mode . "## Reqest\n")))
  (gptel-response-prefix-alist . '((markdown-mode . "## Response\n")
                                 ;; (org-mode . "<< Response >>\n")
                                 (org-mode . "** Response\n")
                                 (text-mode . "<< Response >>\n")))
  (gptel-display-buffer-action . '(pop-to-buffer-same-window)))

;;;;----------------------------------------------------------------
;;;; Major modes/Language config
;;;;----------------------------------------------------------------
(leaf *dired
  :config
  (leaf dired
    :custom
    (dired-listing-switches . "-alh")
    (dired-kill-when-opening-new-dired-buffer . t)
    :hook
    (dired-mode-hook . hl-line-mode)
    (dired-mode-hook . (lambda () (display-line-numbers-mode -1))))
  (leaf dired-sidebar :ensure t)
  (leaf nerd-icons-dired
    :ensure t
    :hook dired-mode-hook))

(leaf help
  :custom
  (help-window-select . t)
  (help-window-keep-selected . t))

(leaf org
  :ensure t
  :custom
  (org-startup-truncated . nil)
  (org-startup-indented . t)
  (org-indent-mode-turns-on-hiding-stars . t)
  (org-startup-with-inline-images . t)
  (org-src-preserve-indentation . t)
  (org-todo-keywords . '((sequence "TODO" "DOING" "DONE")))
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

(leaf org-refile
  :init
  (defun my/org-refile-target ()
    (directory-files "~/Documents/org" t ".*\\.org$"))
  :custom
  (org-refile-targets . '((my/org-refile-target :maxlevel . 3))))

(leaf org-modern
  :ensure t
  :hook org-mode-hook
  :custom
  (org-modern-star . 'replace)
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))

(leaf org-journal
  :ensure t
  :custom
  (org-journal-dir . "~/Documents/org/journal")
  (org-journal-date-format . "%Y-%m-%d")
  (org-journal-mode-hook . nil)         ; disable visual-line-mode
  (org-journal-find-file . 'find-file)
  (org-journal-carryover-items . "TODO=\"TODO\"|TODO=\"DOING\""))

(leaf python-pytest
  :ensure t
  :config
  (defun my/python-pytest--extra-process-sentinel (proc event)
    "Beeps when error, and always switch to the window"
    (when (memq (process-status proc) '(exit signal))
      (let ((exit-code (process-exit-status proc)))
        (when (/= exit-code 0) (beep))
        (switch-to-buffer-other-window (process-buffer proc)))))
  (advice-add 'python-pytest--process-sentinel :after #'my/python-pytest--extra-process-sentinel)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _)
                   (string-match-p "\\*pytest\\*" bufname))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.4)))
  :custom
  (python-pytest-executable . "uv run pytest -vv")
  (python-pytest-unsaved-buffers-behavior . 'save-all))

(leaf vue-mode
  :ensure t
  :hook
  ;; https://github.com/AdamNiederer/vue-mode/issues/74#issuecomment-577338222
  (vue-mode-hook . (lambda () (setq syntax-ppss-table nil)))
  (vue-mode-hook . prettier-mode))

(leaf js-ts-mode
  :custom
  (js-indent-level . 2))

(leaf *javascript
  :config
  (leaf prettier
    :ensure t
    :hook
    (js-mode-hook . prettier-mode)
    :custom
    (prettier-prettify-on-save-flag . nil)))

(leaf go-ts-mode
  :custom
  (go-ts-mode-indent-offset . 4)) ; align with tab-width of 4

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

(leaf makefile-gmake-mode
  :mode "[Mm]akefile\\'")

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
                           (fullscreen . fullheight)))
  :config
  ;; don't show default something
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0))

(leaf window
  :custom
  ; split to bottom if window height is at least this value
  (split-height-threshold . 999))

(leaf *only-ns
  :when (eq window-system 'ns)
  :config
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;; (add-to-list 'default-frame-alist '(ns-appearance . dark))  ;; Make fontface white for dark titelbar
  )

(leaf *font-config
  :config
  (set-language-environment "Japanese")
  (create-fontset-from-ascii-font "Cica-16" nil "mydefault") ; Create a fontset for ASCII
  (set-fontset-font "fontset-mydefault" nil "Cica") ; Extend coverage for other charset
  (set-fontset-font "fontset-mydefault" 'emoji "Apple Color Emoji") ; Override for emojis
  (set-fontset-font "fontset-mydefault" 'nil "Iosevka Term" nil 'append) ; Fallback
  (add-to-list 'default-frame-alist '(font . "fontset-mydefault"))
  (set-face-attribute 'default nil :font "fontset-mydefault")
  ;; We need to set font family of fixed-pitch face directly, not using fontset, to support
  ;; variable-pitch-mode
  (set-face-attribute 'fixed-pitch nil :family "Cica"))
(leaf *theme-config
  :config
  (leaf modus-themes
    :ensure t
    :when (window-system)
    :custom
    (modus-themes-mixed-fonts . t)
    (modus-themes-bold-constructs . t)
    (modus-themes-italic-constructs . t)
    (modus-themes-headings
     . '((1 . (1.35))
         (2 . (1.3))
         (3 . (1.2))
         (4 . (1.1))
         (5 . (1.1))))
    (modus-operandi-palette-overrides
     . '(
         ;; Anthropic
         (bg-main            "#faf9f5")
         (bg-dim             "#f5f4ed")
         (bg-inactive        "#e8e6dc")
         (bg-active          "#e3dacc")
         ;; basic
         (fg-main          "#141414")
         (fg-dim           "#888888")
         ;; Common accents from standard-light-theme
         (red             "#b3303a")
         (red-warmer      "#e00033")
         (red-cooler      "#ce2b50")
         (red-faint       "#b22222")
         (green           "#228b22")
         (green-warmer    "#4f7400")
         (green-cooler    "#008858")
         (green-faint     "#61756c")
         ;; (yellow          "#a45f22")
         ;; (yellow-warmer   "#b6532f")
         ;; (yellow-cooler   "#a0522d")
         ;; (yellow-faint    "#76502a")
         (yellow          "#8b8b00") ; updated
         (yellow-warmer   "#eaa000") ; updated
         (yellow-cooler   "#b5b522") ; updated
         (yellow-faint    "#8b8b45") ; updated
         (blue            "#001faf")
         (blue-warmer     "#3a5fcd")
         (blue-cooler     "#0000ff")
         (blue-faint      "#483d8b")
         ;; (magenta         "#721045")
         ;; (magenta-warmer  "#8b2252")
         ;; (magenta-cooler  "#800080")
         ;; (magenta-faint   "#8f4499")
         (magenta         "#e91c7a") ; updated
         (magenta-warmer  "#ff1a9e") ; updated
         (magenta-cooler  "#d946b8") ; updated
         (magenta-faint   "#b8276e") ; updated
         (cyan            "#1f6fbf")
         (cyan-warmer     "#2f8fab")
         (cyan-cooler     "#008b8b")
         (cyan-faint      "#3f7a80")
         ;; colors from modus-themes-operandi-deuteranopia-palette
         (bg-added           "#d5d7ff")
         (bg-added-faint     "#e6e6ff")
         (bg-added-refine    "#babcef")
         (bg-added-fringe    "#275acc")
         (fg-added           "#303099")
         (fg-added-intense   "#0303cc")
         (bg-changed         "#eecfdf")
         (bg-changed-faint   "#f0dde5")
         (bg-changed-refine  "#e0b0d0")
         (bg-changed-fringe  "#9f6ab0")
         (fg-changed         "#6f1343")
         (fg-changed-intense "#7f0f9f")
         (bg-removed         "#f4f099")
         (bg-removed-faint   "#f6f6b7")
         (bg-removed-refine  "#ede06f")
         (bg-removed-fringe  "#c0b200")
         (fg-removed         "#553d00")
         (fg-removed-intense "#7f6f00")
         ;; adjust tone
         (fringe                  unspecified)
         (border                  bg-inactive)
         (bg-tab-current          bg-main)
         (bg-line-number-active   bg-inactive)
         (bg-line-number-inactive unspecified)
         (bg-mode-line-active     bg-inactive)
         (bg-mode-line-inactive   bg-dim)
         (modeline-err            yellow-faint)
         (bg-region               bg-cyan-intense)
         (bg-diff-context         bg-inactive)
         (docstring   fg-alt)
         (fnname      blue)
         (string      blue-warmer)
         (type        yellow)
         (variable    yellow)
         (fg-heading-1  fg-alt)
         (fg-heading-2  fg-alt)
         (fg-heading-3  fg-alt)
         (fg-heading-4  fg-alt)
         (fg-heading-5  fg-alt)
         (fg-heading-6  fg-alt)
         (fg-heading-7  fg-alt)
         (fg-heading-8  fg-alt)
         ))
    :config
    (load-theme 'modus-operandi :no-confirm))

  (leaf mmm-mode
    :custom
    (mmm-submode-decoration-level . 0)))

(leaf *appearance-config
  :custom
  (indent-tabs-mode . nil)   ; use spaces
  (tab-width . 4)            ; default is 8
  (fill-column . 100)
  :hook
  (prog-mode-hook . display-line-numbers-mode)
  (sgml-mode-hook . display-line-numbers-mode)
  ;; Use :init instead of :config when using with :hook
  ;; :config with :hook adds eval-after-load and makes unintended behavior
  :init
  (show-paren-mode t)
  (leaf variable-pitch
    :hook (markdown-mode-hook org-mode-hook))
  (leaf perfect-margin :ensure t)
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
    (doom-modeline-major-mode-color-icon . t)
    (doom-modeline-vcs-max-length . 24)
    (doom-modeline-enable-word-count . t)
    (doom-modeline-buffer-encoding . 'nondefault)
    (doom-modeline-buffer-file-name-style . 'relative-from-project)
    (doom-modeline-height . 28)
    (doom-modeline-env-version . nil)
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

;;;;----------------------------------------------------------------
;;;; Keys
;;;;----------------------------------------------------------------
(leaf *keybindings
  :config
  (leaf *mouse
    :config
    (global-unset-key [mouse-2]))       ; Disable middle click
  (leaf *key-delete-with-c-h
    :bind
    ("C-h" . delete-backward-char)
    (isearch-mode-map
     ("C-h" . isearch-delete-char)))
  (leaf *misc
    :bind
    ("M-h" . backward-kill-word)
    ("C-x C-k" . kill-current-buffer)
    ("C-x C-b" . ibuffer)
    ("M-i" . completion-at-point))
  (leaf isearch
    :bind
    ("C-S-s" . isearch-forward-symbol-at-point))
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
  (leaf undo-fu
    :bind
    ("C-z" . nil)
    ("C-z" . undo-fu-only-undo)
    ("M-z" . undo-fu-only-redo))
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
  (leaf org-journal
    :bind
    ("C-c j" . org-journal-new-entry))
  (leaf python-pytest
    :bind
    (python-ts-mode-map
     :package python
     ("C-c t" . python-pytest-dispatch)))
  (leaf view
    :bind
    (view-mode-map
     ("h" . View-scroll-line-forward)
     ("t" . View-scroll-line-backward)
     ("H" . View-scroll-half-page-forward)
     ("T" . View-scroll-half-page-backward)))
  (leaf consult
    :bind
    ("M-s" . consult-line)
    ("M-S" . my/consult-line-symbol-at-point)
    ("s-s" . consult-ripgrep)
    ("s-S" . my/consult-ripgrep-symbol-at-point)
    ("C-x b" . consult-buffer)
    ("C-x C-r" . consult-recent-file)
    ("M-g i" . consult-imenu)
    ("M-y" . consult-yank-from-kill-ring)
    ([remap projectile-switch-to-buffer] . consult-project-buffer) ; M-p b
    )
  (leaf xref
    :bind
    ("M-'" . xref-find-references))
  (leaf embark
    :bind
    ("s-e" . embark-act))
  (leaf marginalia
    :bind (minibuffer-local-map
           ("M-A" . marginalia-cycle)))
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
  (leaf lsp
    :bind
    ("M-I" . lsp-ui-doc-show))
  (leaf vterm
    :bind
    ("<f2>" . vterm-toggle)
    ("s-j" . vterm-toggle)
    (vterm-mode-map
     ("C-<f2>" . my/vterm-new-buffer-in-current-window)
     ("C-<return>" . vterm-toggle-insert-cd)
     ;; ([remap projectile-previous-project-buffer] . vterm-toggle-forward)
     ;; ([remap projectile-next-project-buffer] . vterm-toggle-backward)
     ))
  (leaf copilot
    :bind
    ("C-c M-f" . 'copilot-complete)
    (copilot-completion-map
     ("C-c M-f" . 'copilot-complete)
     ("C-g" . 'copilot-clear-overlay)
     ("M-n" . 'copilot-next-completion)
     ("M-p" . 'copilot-previous-completion)
     ("C-<return>" . 'copilot-accept-completion)
     ("M-f" . 'copilot-accept-completion-by-word)
     ("M-<return>" . 'copilot-accept-completion-by-line)))
  (leaf transpose-frame
    :bind
    ("C-x C-t" . 'transpose-frame))
  (leaf gptel
    :bind
    (gptel-mode-map
     ("C-c C-<return>" . 'gptel-send)
     ("C-x C-s" . 'my/save-gptel)))
  (leaf open-junk-file
    :bind
    ("C-x j" . open-junk-file))
  (leaf macrostep
    :bind
    (emacs-lisp-mode-map
     :package elisp-mode
     ("C-c e" . macrostep-expand)))
  (leaf dired-sidebar
    :bind
    ("C-x C-n" . dired-sidebar-toggle-sidebar)))

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
