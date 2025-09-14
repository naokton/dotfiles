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
  :custom-face
  (tab-bar . '((t (:height 1.0 :box (:line-width 1)))))
  (tab-bar-tab . '((t (:weight bold :box (:line-width 1)))))
  (tab-bar-tab-inactive . '((t (:box (:line-width 1 :color "black") :background "#dfdfdf"))))
  :config
  (tab-bar-mode)
  (defun my/project-name-or-default ()
    "Return project name when in project, or default tab-bar tab name"
    (let ((project-name (projectile-project-name)))
      (if (string= "-" project-name)
          (tab-bar-tab-name-current)
        (projectile-project-name))))
  )

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
  ;; Completion style
  :ensure t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion))))
  )

(leaf consult
  :ensure t
  :config
  (leaf consult-flycheck :ensure t)
  (leaf xref
    :custom
    (xref-prompt-for-identifier . nil)
    (xref-show-xrefs-function . #'consult-xref)
    (xref-show-definitions-function . #'consult-xref))
  (defun my/consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  :defer-config
  (consult-customize
   consult-recent-file
   :preview-key '(:debounce 0.2 any)))

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
  ;; :config
  ;; ;; Workaround of not working counsel-yank-pop
  ;; ;; https://github.com/akermu/emacs-libvterm#counsel-yank-pop-doesnt-work
  ;; (defun my/vterm-counsel-yank-pop-action (orig-fun &rest args)
  ;;   (if (equal major-mode 'vterm-mode)
  ;;       (let ((inhibit-read-only t)
  ;;             (yank-undo-function (lambda (_start _end) (vterm-undo))))
  ;;         (cl-letf (((symbol-function 'insert-for-yank)
  ;;                    (lambda (str) (vterm-send-string str t))))
  ;;           (apply orig-fun args)))
  ;;     (apply orig-fun args)))
  ;; (advice-add 'counsel-yank-pop-action :around #'my/vterm-counsel-yank-pop-action)
  )

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

(leaf ultra-scroll
  :ensure t
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
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
  ;; https://github.com/emacs-lsp/lsp-mode/issues/4313
  (with-eval-after-load 'lsp-volar
    (lsp-dependency 'typescript '(:system "/opt/homebrew/bin/tsserver")))
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
  :hook
  (python-ts-mode-hook . (lambda ()
                           (require 'lsp-pyright)
                           (lsp-deferred))))

(leaf lsp-ui
  :ensure t
  :hook
  ;; Prevent rings when hovering mouse over the tab bar. https://github.com/emacs-lsp/lsp-ui/issues/681
  (lsp-after-initialize-hook . (lambda () (local-set-key (kbd "<tab-bar> <mouse-movement>") #'ignore))))

(leaf *ediff
  :setq
  (ediff-window-setup-function . 'ediff-setup-windows-plain)
  (ediff-split-window-function . 'split-window-horizontally))

(leaf magit
  :ensure t
  :custom
  (magit-display-buffer-function . 'magit-display-buffer-same-window-except-diff-v1)
  )

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
  :vc (:url "https://github.com/copilot-emacs/copilot.el")
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
  (copilot-chat-model . "claude-3.5-sonnet"))

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

(leaf rainbow-mode :ensure t)

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
  (gptel-model . 'claude-3-5-sonnet-20241022)
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
  (org-indent-mode-turns-on-hiding-stars . nil)
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

(leaf org-modern
  :ensure t
  :hook org-mode-hook)

(leaf org-modern-indent
  :vc (:url "https://github.com/jdtsmith/org-modern-indent")
  :hook org-mode-hook)

(leaf org-journal
  :ensure t
  :custom
  (journal-dir . "~/Documents/journal")
  (journal-date-format . "%Y-%m-%d")
  (org-journal-find-file . 'find-file)
  (org-journal-carryover-items . "TODO=\"TODO\"|TODO=\"DOING\"")
  (org-journal-handle-old-carryover . '(lambda (_) nil)) ; leave carryover items as is in the prev day journal
  )

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
                           (height . 80)))
  :config
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;; don't show default something
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0))

(leaf *font-confnig
  :config
  (set-language-environment "Japanese")
  (create-fontset-from-ascii-font "Cica-16" nil "mydefault") ; Create a fontset for ASCII
  (set-fontset-font "fontset-mydefault" nil "Cica") ; Extend coverage for other charset
  (set-fontset-font "fontset-mydefault" 'emoji "Apple Color Emoji") ; Override for emojis
  (set-fontset-font "fontset-mydefault" 'nil "Iosevka Term" nil 'append) ; Fallback
  (add-to-list 'default-frame-alist '(font . "fontset-mydefault"))
  (set-face-attribute 'default nil :font "fontset-mydefault")
  (set-face-attribute 'fixed-pitch nil :font (face-attribute 'default :font)))

(leaf *theme-config
  :config
  (leaf modus-themes
    :ensure t
    :custom
    (modus-themes-to-toggle . '(modus-operandi-tinted modus-vivendi-tinted))
    (modus-themes-italic-constructs . t)
    (modus-themes-bold-constructs . t)
    (modus-themes-mixed-fonts . t)
    (modus-themes-headings . '((1 . (1.2))
                               (2 . (1.2))
                               (t . (1.1)))))
    :config
    (load-theme 'modus-operandi-tinted :no-confirm)
    (modus-themes-with-colors
      ;; override theme faces not defined in modus-theme
      (custom-set-faces
       `(lsp-ui-doc-background ((,c :background ,bg-dim))))
      (customize-set-variable 'lsp-ui-doc-border border))
  (leaf mmm-mode
    :custom
    (mmm-submode-decoration-level . 0)))

(leaf *appearance-config
  :custom
  (indent-tabs-mode . nil)   ; use spaces
  (tab-width . 4)            ; default is 8
  (fill-column . 100)
  :config
  (show-paren-mode t)
  (global-display-line-numbers-mode t)
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
    (doom-modeline-check-simple-format . t)
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
