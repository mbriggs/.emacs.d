;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my v3 Emacs configuration, and the first time I've used holy Emacs.
;; Using as much of the built in functionality as possible. Naviagation of this
;; file is intended to happen via consult-outline (M-g o).

;; On install, run `M-x nerd-icons-install-fonts` to install the nerd fonts for
;; the icons in the modeline.

;;; Code:

(use-package package ; package setup
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(use-package emacs ; core configuration
  :bind (("M-o" . project-find-file)
	 ("M-O" . other-window)
	 ("M-e" . consult-project-buffer)
	 ("M-E" . consult-buffer))

  :config

  ;; Mac uses cmd as meta
  (when (eq system-type 'darwin)
    (setq
     mac-command-modifier 'meta
     mac-option-modifier 'super
     mac-right-command-modifier 'control))

  (setq
   ;; stop the beeping
   visible-bell t
   ring-bell-function 'ignore
   ;; modeline info
   line-number-mode t
   column-number-mode t
   ;; ui settings
   x-underline-at-descent-line t ; Prettier underlines
   switch-to-buffer-obey-display-actions t ; Make switching buffers more consistent
   show-trailing-whitespace nil ; By default, don't underline trailing spaces
   indicate-buffer-boundaries 'left ; Show buffer top and bottom in the margin
   mouse-wheel-tilt-scroll t ; Enable horizontal scrolling
   mouse-wheel-flip-direction t ; Natural scrolling
   create-lockfiles nil ; Don't create lockfiles
   indent-tabs-mode t ; Use tabs
   tab-width 4 ; Set the tab width to 4
   inhibit-startup-screen t ; No splash screen
   sentence-end-double-space nil ; Don't require two spaces to end a sentence
   backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))) ; Store backups in a separate directory
   auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave" user-emacs-directory) t)) ; Store autosaves in a separate directory
   backup-by-copying t ; Copy files when backing up
   delete-old-versions t ; Delete old versions of files
   kept-new-versions 6 ; Keep 6 new versions
   version-control t ; Use version control
   auto-save-default t ; Autosave files
   inhibit-startup-message t ; Don't show the startup message
   initial-major-mode 'emacs-lisp-mode ; Start in elisp mode
   fill-column 85 ; Set the fill column
   uniquify-buffer-name-style 'forward ; Uniquify buffer names
   uniquify-separator ":" ; Uniquify buffer names
   require-final-newline t ; Always end files with a newline
   password-cache-expiry (* 60 15) ; Don't expire passwords
   display-time-mode nil ; Don't show the time in the mode line
   )

  ;; theme
  (set-frame-font "JetBrains Mono 14" nil t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; misc ui settings
  (blink-cursor-mode -1)                                ; Steady cursor
  (pixel-scroll-precision-mode)                         ; Smooth scrolling
  (repeat-mode) ; Repeat common commands with a single key press
  (fset 'yes-or-no-p 'y-or-n-p)                          ; y/n instead of yes/no
  (show-paren-mode)                                      ; Highlight matching parens

  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))

  ;; auto reload file
  (setopt auto-revert-interval 5)
  (setopt auto-revert-check-vc-info t)
  (global-auto-revert-mode)

  ;; Display line numbers in programming mode
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (setopt display-line-numbers-width 3)           ; Set a minimum width

  ;; Nice line wrapping when working with text
  (add-hook 'text-mode-hook 'visual-line-mode)

  ;; Modes to highlight the current line with
  (let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
    (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

  ;; add the lisp directory to the load path, for my code
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  ;; add the lisp directory to the load path, for non repo third party code
  (add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

  ;; when splitting windows, focus on the new window
  (defadvice split-window (after focus-new-window activate)
    (select-window (get-lru-window)))
  )

(use-package mb-op ; one-password integration for secrets
  :functions mb-op-register-reference
  :commands mb-op-read
  :config
  (mb-op-register-reference 'gist-token "op://Personal/lrian52eh4tkos2lgabjmkh6ve/credential"))

(use-package jist ; gist integration
  :bind ("C-x g j" . jist-dwim)
  :ensure t
  :commands (jist-auth-buffer jist-auth-region jist-auth-buffer-public jist-auth-region-public)
  :defines jist-github-token jist-enable-default-authorized
  :init
  (defun jist-dwim ()
    "DWIM for create gist current buffer or region."
    (interactive)
    (if (region-active-p)))
  :config
  (setq
   jist-github-token (mb-op-read 'gist-token)
   jist-enable-default-authorized t))


(use-package catppuccin-theme ; my favourite theme, both light and dark variants
  :ensure t
  :defines catppuccin-flavor
  :functions catppuccin-reload
  :config
  (setq catppuccin-flavor 'frappe)
  (catppuccin-reload))

(use-package auto-dark ; set theme based on osx system state
  :ensure t
  :defines (auto-dark-allow-osascript auto-dark-light-theme auto-dark-dark-theme)
  :functions auto-dark-mode
  :config
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-light-theme 'catppuccin)
  (setq auto-dark-dark-theme 'catppuccin)

  (add-hook 'auto-dark-dark-mode-hook
	    (lambda ()
	      (setq catppuccin-flavor 'frappe)
	      (catppuccin-reload)
	      (modify-frame-parameters nil '((ns-transparent-titlebar . t)
					     (ns-appearance . dark)))

	      ))

  (add-hook 'auto-dark-light-mode-hook
	    (lambda ()
	      (setq catppuccin-flavor 'latte)
	      (catppuccin-reload)
	      (modify-frame-parameters nil '((ns-transparent-titlebar . t)
					     (ns-appearance . light)))
	      ))

  (auto-dark-mode t))

(use-package eat ; use eshell for most stuff, eat for tui style
  :ensure t
  :functions eat-eshell-mode eat-eshell-visual-command-mode
  :config

  (setopt eat-kill-buffer-on-exit t)

  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(use-package highlight-defined ; highlight defined symbols
  :ensure t
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package whole-line-or-region ; commands work on the current line if no region is selected
  :ensure t
  :functions whole-line-or-region-global-mode
  :config
  (whole-line-or-region-global-mode))

(use-package tab-bar ; enable the tab-bar
  :init
  (setopt tab-bar-show 1)
  :config
  (tab-bar-mode 1)

  ;; Add the time to the tab-bar, if visible
  (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
  (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
  (setopt display-time-format "%a %F %T")
  (setopt display-time-interval 1)
  (display-time-mode))

(use-package feature-mode ; feature mode for cucumber
  :mode ("\.feature$" . feature-mode)
  :ensure t)

(use-package recentf ; track recent files
  :init
  (setopt recentf-max-saved-items 100)
  (setopt recentf-auto-cleanup 'never)
  :config
  (recentf-mode))

(use-package which-key ; show keybindings
  :ensure t
  :functions which-key-mode
  :config
  (which-key-mode))

(use-package mb-fortune ; show an inspirational message in the scratch buffer
  :functions mb-fortune
  :config
  (setq initial-scratch-message (concat ";; " (mb-fortune) "\n\n")))

(use-package git-link ; get a link to the current line in the current file
  :ensure t
  :commands git-link
  :bind ("C-x g k" . git-link))

(use-package magit ; git porcelain
  :ensure t
  :commands magit-status magit-blame-mode
  :bind (("C-x g g" . magit-status)
	 ("C-x g b" . magit-blame)
	 ("C-x g l" . magit-log-buffer-file)
	 ("C-x g L" . magit-log-all)))

(use-package git-gutter ; show git changes in the fringe
  :ensure t
  :requires (git-gutter-fringe fringe-helper)
  :functions global-git-gutter-mode
  :config
  ;; (require 'git-gutter-fringe)
  ;; (require 'git-gutter)
  (if (fboundp 'fringe-mode) (fringe-mode '4))

  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  (global-git-gutter-mode))

(use-package exec-path-from-shell ; path from instanciated shell
  :ensure t
  :functions exec-path-from-shell-initialize
  :config
  (exec-path-from-shell-initialize))

(use-package wgrep ; edit grep results
  :ensure t)

(use-package doom-modeline
  :ensure t
  :functions doom-modeline-mode
  :defines doom-modeline-project-detection
  :config
  (setq doom-modeline-project-detection 'project)
  (display-time-mode -1)
  (doom-modeline-mode 1))

(use-package nerd-icons
  :ensure t)

(use-package flymake ; on the fly syntax checking
  :hook (prog-mode . flymake-mode))

;; TODO: swap this as soon as someone makes a childframe impl for flymake
(use-package flymake-popon ; show flymake errors in a childframe
  :ensure t
  :hook (flymake-mode . flymake-popon-mode))

(use-package eglot ; language server protocol
  :requires jsonrpc
  :hook
  ((ruby-ts-mode . eglot-ensure)
   (yaml-ts-mode . eglot-ensure)
   (typescript-ts-mode .eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (web-mode . eglot-ensure)
   (bash-ts-mode . eglot-ensure)
   (html-ts-mode . eglot-ensure)
   (css-ts-mode . eglot-ensure)
   (json-ts-mode . eglot-ensure)
   (cmake-ts-mode . eglot-ensure)
   (go-ts-mode . eglot-ensure)
   (dockerfile-ts-mode . eglot-ensure))


  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files
  )

(use-package format-all ; format code on save
  :commands format-all-mode
  :hook ((prog-mode . format-all-mode)
	 (format-all-mode . format-all-ensure-formatter))
  :ensure t
  :config
  (setq-default format-all-formatters
		'(("CSS" prettierd)
		  ("HTML" prettierd)
		  ("JavaScript" prettierd)
		  ("JSON" prettierd)
		  ("JSON5" prettierd)
		  ("JSX" prettierd)
		  ("SCSS" prettierd)
		  ("TSX" prettierd)
		  ("Markdown" prettierd)
		  ("Ruby" rubocop)
		  ("Shell" shfmt)
		  ("SQL" pg_format)
		  ("TypeScript" prettierd)
		  ("YAML" prettierd)
		  ("Go" gofmt)
		  ("Svelte" prettierd)
		  ("TOML" prettierd)
		  ("GraphQL" prettierd))))

(use-package copilot ; code completion using llvm
  :after (editorconfig dash s jsonrpc)
  :load-path ".copilot"
  :commands copilot-mode
  :hook ((prog-mode . copilot-mode)
	 (copilot-mode . (lambda ()
			   (setq-local copilot--indent-warning-printed-p t))))
  :defines (copilot-indentation-alist copilot-completion-map)
  :bind (
	 :map copilot-completion-map
	 ("M-;" . copilot-accept-completion)
	 ("C-TAB" . copilot-accept-completion-by-word)
	 ("C-<tab>" . copilot-accept-completion-by-word)
	 ("C-S-<tab>" . copilot-accept-completion-by-line)
	 )
  :config
  (add-to-list 'copilot-indentation-alist
	       '(lisp-interaction-mode . (lambda () (current-column)))
	       '(emacs-lisp-mode . (lambda () (current-column)))))

(use-package dash ; list manipulation lib
  :ensure t)

(use-package s ; string manipulation lib
  :ensure t)

(use-package jsonrpc ; jsonrpc lib (using a later version from elpa then what is shipped in 29.1)
  :pin gnu
  :load-path "jsonrpc-1.0.24"
  :functions jsonrpc--log-event
  :config
  (fset #'jsonrpc--log-event #'ignore))  ; massive perf boost---don't log every event)

(use-package editorconfig ; editorconfig support
  :ensure t
  :functions editorconfig-mode
  :hook (prog-mode . editorconfig-mode))

(use-package treesit-auto ; auto download and install treesitter grammars
  :ensure t
  :functions (treesit-auto-install treesit-auto-add-to-auto-mode-alist global-treesit-auto-mode)  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package markdown-mode ; markdown mode
  :ensure t)

(use-package web-mode ; web mode for erb
  :ensure t
  :mode (("\\.erb$" . web-mode)))


(use-package vertico ; minibuffer completion
  :ensure t
  :defines vertico-count
  :functions vertico-mode
  :init
  (setq vertico-count 20)
  (vertico-mode))


(use-package vertico-directory ; ido-line directory nav
  :after vertico
  :functions vertico-directory-tidy
  :defines vertico-map
  :init
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  :bind
  (:map vertico-map
	("RET" . vertico-directory-enter)
	("DEL" . vertico-directory-delete-char)
	("M-DEL" . vertico-directory-delete-word)))

(use-package savehist ; save minibuffer history
  :init
  (savehist-mode))

(use-package orderless ; fuzzy matching
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia ; annotations in minibuffer
  :ensure t
  :functions marginalia-mode
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark-consult ; consult integration for embark
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark ; context-based minibuffer actions
  :ensure t
  :functions embark-prefix-help-command
  :bind
  (("C-." . embark-act))

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package consult ; consult is a replacement for the default Emacs completing read
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :defines (consult-project-function consult-ripgrep-command consult-git-grep-command consult-grep-command consult-bookmark-command consult-recent-file-command consult-xref-command consult-register-command consult-customize xref-show-xrefs-function xref-show-definitions-function register-preview-delay register-preview-function consult-preview-key consult-narrow-key consult-narrow-map consult-project-function consult-theme consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-recent-file consult-xref consult--source-bookmark consult--source-file-register consult--source-recent-file consult--source-project-recent-file consult-register-format consult-register-window consult-customize)
  :functions (consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-recent-file consult-xref consult-register-format consult-register-window consult-customize)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  )

(use-package corfu ; in-buffer completion framework
  :ensure t
  :functions global-corfu-mode
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo ; documentation popup
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package corfu-terminal ; corfu for terminal
  :if (not (display-graphic-p))
  :functions corfu-terminal-mode
  :ensure t
  :config
  (corfu-terminal-mode))

(use-package kind-icon ; icons for corfu
  :if (display-graphic-p)
  :functions kind-icon-margin-formatter
  :defines corfu-margin-formatters
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eshell ; eshell as term of choice
  :defines eshell-mode-map
  :commands eshell
  :bind (
	 :map eshell-mode-map
	 ("C-r" . consult-history)))

(use-package cape ; completion at point extensions
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package yaml-mode ; yaml mode
  :ensure t)

(use-package inf-ruby ; inferior ruby mode
  :ensure t
  :bind (("C-c C-i S" . inf-ruby)
	 ("C-c C-i b" . ruby-send-buffer)
	 ("C-c C-i r" . ruby-send-region)
	 ("C-c C-i RET" . ruby-send-dwim)
	 ("C-c C-i i" . ruby-switch-to-inf)
	 ("C-c C-i s" . inf-ruby-console-auto)
	 ("C-c C-i t" . inf-ruby-test))
  :commands inf-ruby-minor-mode
  :hook (ruby-ts-mode . inf-ruby-minor-mode)
  :functions (ruby-send-line ruby-send-region)
  :init
  (defun ruby-send-dwim ()
    "Send the appropriate ruby code to the inferior ruby process."
    (interactive)
    (if (region-active-p)
	(ruby-send-region (region-beginning) (region-end))
      (ruby-send-line))))

(use-package yard-mode ; yard mode for ruby
  :ensure t
  :hook ((ruby-mode ruby-ts-mode) . yard-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(git-link inf-ruby git-timemachine jist feature-mode highlight-defined highlight-defined-mode yaml-mode doom-modeline mini-modeline jsonrpc vertico mmm-mode derived auto-dark eat whole-line-or-region flymake-popon exec-path-from-shell format-all editorconfig s web-mode treesit-auto kind-icon corfu-terminal cape corfu wgrep embark-consult embark marginalia which-key orderless catppuccin-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
