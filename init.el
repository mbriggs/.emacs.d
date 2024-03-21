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

(use-package benchmark-init ; benchmarking, enable to benchmark init
  ;; :disabled
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package isearch
  :bind (("C-s" . isearch)
	 ("M-s s" . isearch)
	 :map isearch-mode-map
	 ("C-s" . mb-isearch-repeat-forward)
	 ("C-r" . mb-isearch-reapeat-backward))
  :init
  (defun mb-isearch-repeat-forward ()
    (interactive)
    (unless isearch-forward
      (goto-char isearch-other-end))
    (isearch-repeat-forward)
    (unless isearch-success
      (isearch-repeat-forward)))

  (defun mb-isearch-repeat-backward ()
    (interactive)
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end))
    (isearch-repeat-backward)
    (unless isearch-success
      (isearch-repeat-backward))))

(use-package emacs ; core configuration
  :bind (("M-o" . project-find-file)
	 ("M-E" . consult-project-buffer)
	 ("M-e" . consult-buffer)
	 ("M-s r" . query-replace)
	 ("M-s R" . query-replace-regexp)
	 ("M-s s" . isearch-forward)
	 ("C-x x e" . erase-buffer)
	 ("C-M-d" . up-list) ; go to the end of the next sexp
	 ("<insert>" . nil) ; nope
	 ("C-z" . nil) ; I have a window manager, thanks!
	 ("C-x C-z" . nil) ; same idea as above
	 ("C-x C-c" . nil) ; avoid accidentally exiting Emacs
	 ("C-x C-c C-c" . #'save-buffers-kill-emacs) ; more cumbersome, less error-prone
	 ("C-h h" . nil) ; Never show that "hello" file
	 ("M-c" . #'capitalize-dwim)
	 ("M-l" . #'downcase-dwim) ; "lower" case
	 ("M-u" . #'upcase-dwim)
	 ("C-h u" . #'apropos-user-option)
	 ("C-h F" . #'apropos-function) ; lower case is `describe-function'
	 ("C-h V" . #'apropos-variable) ; lower case is `describe-variable'
	 ("C-h L" . #'apropos-library) ; lower case is `view-lossage'
	 ("C-h c" . #'describe-char)) ; overrides `describe-key-briefly'
  :config

  ;; Mac uses cmd as meta
  (when (eq system-type 'darwin)
    (setq
     mac-command-modifier 'meta
     mac-option-modifier 'super
     mac-right-command-modifier 'control))

  ;;; Runtime optimizations (stolen from doom)
  ;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  (setq auto-mode-case-fold nil)

  ;; PERF: Disable bidirectional text scanning for a modest performance boost.
  ;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
  ;;   say that is an undefined state and suggest this to be just as good:
  (setq-default bidi-display-reordering 'left-to-right
		bidi-paragraph-direction 'left-to-right)

  ;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
  ;;   reordering of bidirectional text with embedded parentheses (and other
  ;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
  (setq bidi-inhibit-bpa t)  ; Emacs 27+ only

  ;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
  ;; in non-focused windows.
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)

  ;; Disable;; Don't ping things that look like domain names.
  (setq-default ffap-machine-p-known 'reject)

  ;; Font compacting can be terribly expensive, especially for rendering icon
  ;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
  ;; hasn't been determined, but do it anyway, just in case. This increases memory
  ;; usage, however!
  (setq inhibit-compacting-font-caches t)

  ;; Increase how much is read from processes in a single chunk (default is 4kb).
  ;; This is further increased elsewhere, where needed (like our LSP module).
  (setq read-process-output-max (* 64 1024))  ; 64kb

  (setq
   ;; stop the beeping
   visible-bell t
   ring-bell-function 'ignore
   ;; modeline info
   line-number-mode t
   column-number-mode t
   ;; everything else
   x-underline-at-descent-line t ; Prettier underlines
   switch-to-buffer-obey-display-actions t ; Make switching buffers more consistent
   show-trailing-whitespace nil ; By default, don't underline trailing spaces
   indicate-buffer-boundaries 'left ; Show buffer top and bottom in the margin
   mouse-wheel-tilt-scroll t ; Enable horizontal scrolling
   mouse-wheel-flip-direction t ; Natural scrolling
   create-lockfiles nil ; Don't create lockfiles
   indent-tabs-mode nil ; don't use tabs
   tab-width 2 ; Set the tab width to 2
   sentence-end-double-space nil ; Don't require two spaces to end a sentence
   backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))) ; Store backups in a separate directory
   auto-save-file-name-transforms `((".*" ,(concat (expand-file-name "autosave" user-emacs-directory) "/\\1") t)) ; Store autosaves in a separate directory
   backup-by-copying t ; Copy files when backing up
   delete-old-versions t ; Delete old versions of files
   kept-new-versions 6 ; Keep 6 new versions
   version-control t ; Use version control
   auto-save-default t ; Autosave files
   inhibit-startup-screen t ; No splash screen
   inhibit-startup-message t ; Don't show the startup message
   inhibit-startup-echo-area-message user-login-name ; dont show the echo message
   inhibit-splash-screen t ; dont show the splash screen
   initial-major-mode 'emacs-lisp-mode ; Start in elisp mode
   use-file-dialog nil ; dont use a dialog to ask about unsaved files
   use-short-answers t ; y or n vs yes and no
   fill-column 85 ; Set the fill column
   uniquify-buffer-name-style 'forward ; Uniquify buffer names
   uniquify-separator ":" ; Uniquify buffer names
   require-final-newline t ; Always end files with a newline
   password-cache-expiry (* 60 15) ; Don't expire passwords
   display-time-mode nil ; Don't show the time in the mode line
   completion-cycle-threshold 1 ; show popup unless only 1 match
   completions-detailed t ; show detailed completions
   completion-auto-help t ; show completions help
   completions-group t ; group completions
   help-window-select t ; always select help windows
   save-interprogram-paste-before-kill t ; clipboard into kill ring
   set-mark-command-repeat-pop t ; repeat mode for C-u C-SPC
   )

  ;; Create the backup and autosave directories if they don't exist
  (dolist (dir '("backups" "autosave"))
    (let ((path (expand-file-name dir user-emacs-directory)))
      (unless (file-exists-p path)
	(make-directory path))))

  ;; Contrary to what many Emacs users have in their configs, you don't need more
  ;; than this to make UTF-8 the default coding system:
  (set-language-environment "UTF-8")
  ;; ...but `set-language-environment' also sets `default-input-method', which is
  ;; a step too opinionated.
  (setq default-input-method nil)

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
  (pixel-scroll-precision-mode)                         ; Smooth scrolling
  (delete-selection-mode 1) ; text replaces region

  ;; Make native compilation silent and prune its cache.
  (when (native-comp-available-p)
    (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
    (setq native-compile-prune-cache t)) ; Emacs 29

  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))

  ;; add the lisp directory to the load path, for my code
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  ;; add the lisp directory to the load path, for non repo third party code
  (add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

  ;; when splitting windows, focus on the new window
  (defadvice split-window (after focus-new-window activate)
    (select-window (get-lru-window)))
  )

(use-package windmove
  :hook ((after-init . windmove-default-keybindings)
	 (after-init . windmove-swap-states-default-keybindings)))

(use-package show-paren ; show matching parens
  :hook (prog-mode . show-paren-mode))

(use-package repeat ; repeat common commands with a single key
  :hook (after-init . repeat-mode))

(use-package compile
  :custom
  (compilation-scroll-output t))

(use-package display-line-numbers ; display line numbers
  :disabled t
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-width 3))

(use-package gcmh ; garbage collection
  :ensure t
  :hook (after-init . gcmh-mode)
  :defines gcmh-idle-delay gcmh-auto-idle-delay-factor gcmh-high-cons-threshold
  :config
  ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
  ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
  ;; when it's idle. However, if the idle delay is too long, we run the risk of
  ;; runaway memory usage in busy sessions. If it's too low, then we may as well
  ;; not be using gcmh at all.
  (setq gcmh-idle-delay 'auto  ; default is 15s
	gcmh-auto-idle-delay-factor 10
	gcmh-high-cons-threshold (* 16 1024 1024)))


(use-package auto-revert ; auto revert files
  :defer 5
  :defines auto-revert-interal auto-revert-check-vc-info
  :config
  (global-auto-revert-mode)
  (setq auto-revert-interal 5
	auto-revert-check-vc-info t))

(use-package mb-op ; one-password integration for secrets
  :functions mb-op-register-reference
  :commands mb-op-read
  :config
  (mb-op-register-reference 'gist-token "op://Personal/lrian52eh4tkos2lgabjmkh6ve/credential")
  (mb-op-register-reference 'gpt-api-key "op://Personal/ChatGPT API Key/credential"))

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
  :init
  (setopt catppuccin-flavor 'frappe)
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package auto-dark ; set theme based on osx system state DISABLED DUE TO POOR PERF AT LOAD
  :ensure t
  :disabled
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

(use-package expreg ; expand region
  :ensure t
  :bind (("<M-up>" . expreg-expand)
	 ("<M-down>" . expreg-contract)))

(use-package surround ; surround text with a key
  :ensure t
  :bind-keymap ("C-q" . surround-keymap))

(use-package sh-script
  :defer t
  :mode (("\\.zsh\\'" . bash-ts-mode)
	 ("\\.zshrc\\'" . bash-ts-mode)
	 ("\\.zshenv\\'" . bash-ts-mode)
	 ("\\.zprofile\\'" . bash-ts-mode)
	 ("\\.zlogin" . bash-ts-mode)
	 ("\\.envrc\\'" . bash-ts-mode)
	 ("\\.envrc.local\\'" . bash-ts-mode)))

(use-package eat ; use eshell for most stuff, eat for tui style
  :ensure t
  :functions eat-eshell-mode eat-eshell-visual-command-mode
  :defines eat-mode-map eat-semi-char-non-bound-keys eat-semi-char-mode-map
  :commands eat-eshell-mode eat-eshell-visual-command-mode
  :bind (("C-x c" . eat)
	 :map eat-mode-map
	 ("S-<left>" . windmove-left)
	 ("S-<right>" . windmove-right)
	 ("S-<up>" . windmove-up)
	 ("S-<down>" . windmove-down)
	 :map eat-semi-char-mode-map
	 ("M-e" . consult-buffer)
         ("M-o" . project-find-file))
  :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-term-name "xterm-256color")
  :config
  (add-to-list 'eat-semi-char-non-bound-keys [M-e]))

(use-package highlight-defined ; highlight defined symbols
  :ensure t
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package whole-line-or-region ; commands work on the current line if no region is selected
  :ensure t
  :functions whole-line-or-region-global-mode
  :config
  (whole-line-or-region-global-mode))

(use-package tab-bar ; enable the tab-bar
  :bind (("M-`" . tab-bar-switch-to-next-tab)
	 ("M-~" . tab-bar-switch-to-prev-tab))
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
  :defer 2
  :init
  (setopt recentf-max-saved-items 100)
  (setopt recentf-keep '(file-remote-p file-readable-p))
  :config
  (recentf-mode 1))

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

(use-package git-gutter ; show git changes in buffers
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:disabled-modes '(org-mode asm-mode image-mode))
  (git-gutter:update-interval 1)
  (git-gutter:window-width 2)
  (git-gutter:ask-p nil))

(use-package git-gutter-fringe ; git gutter in the fringe
  :ensure t
  :diminish git-gutter-mode
  :after git-gutter
  :demand fringe-helper
  :config
  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center))

(use-package exec-path-from-shell ; path from instanciated shell
  :ensure t
  :functions exec-path-from-shell-initialize
  :config
  (setopt exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package proced ; something like top
  :custom
  (proced-auto-update-flag t)
  (proced-enable-color-flag t)
  (proced-auto-update-interval 5)
  (proced-descend t)
  (proced-filter 'user))

(use-package wgrep ; edit grep results
  :ensure t)

(use-package doom-modeline ; fancy modeline
  :ensure t
  :functions doom-modeline-mode
  :config
  (setopt
   doom-modeline-project-detection 'project
   doom-modeline-buffer-file-name-style 'relative-from-project
   doom-modeline-buffer-state-icon nil
   doom-modeline-env-version nil
   doom-modeline-buffer-encoding nil
   )
  (display-time-mode -1)
  (doom-modeline-mode 1))

(use-package nerd-icons ; icons in emacs
  :ensure t)

(use-package nerd-icons-dired ; icons in dired
  :ensure t
  :after dired
  :functions nerd-icons-dired-mode
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired ; file manager
  :defer t
  :hook ((dired-mode . dired-hide-details-mode)
	 (buffer-list-update . (lambda ()
				 (when (equal major-mode 'dired-mode)
				   (revert-buffer t t t)))))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  :config
  (when (eq system-type 'darwin)
    (setq
     dired-use-ls-dired t
     insert-directory-program "gls"
     dired-listing-switches "-AGFBhl --group-directories-first --time-style=long-iso")))

(use-package dired-subtree ; expand in place
  :ensure t
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("<backtab>" . dired-subtree-remove)
	      ("C-M-u" . dired-subtree-up)
	      ("C-M-d" . dired-subtree-down)
	      ("C-M-f" . dired-subtree-next-sibling)
	      ("C-M-b" . dired-subtree-previous-sibling)))

(use-package wdired ; writable dired
  :ensure t
  :after dired)

(use-package trashed ; trash editor
  :ensure t)

(use-package dired-x ; dired extensions
  :after dired
  :init
  (setopt dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(use-package dired-narrow ; filter dired
  :bind (:map dired-mode-map
	      ("/" . dired-narrow))
  :ensure t
  :after dired)

(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :after elfeed
  :defines rmh-elfeed-org-files org-directory
  :functions elfeed-org
  :config
  (require 'org)
  (setq rmh-elfeed-org-files (list (concat org-directory "/elfeed.org")))
  (elfeed-org))


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
   (typescript-ts-mode . eglot-ensure)
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
  (eglot-extend-to-xref t))

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
		  ("GraphQL" prettierd)))

  ;; hack up rubocop formatter so --server is set
  (define-format-all-formatter rubocop
    (:executable "rubocop")
    (:install "gem install rubocop:'>=1.4.0'")
    (:languages "Ruby")
    (:features)
    (:format
     (format-all--buffer-hard-ruby
      "rubocop" '(0 1) nil nil
      executable
      "--server"
      "--auto-correct"
      "--format" "quiet"
      "--stderr"
      "--stdin" (or (buffer-file-name) (buffer-name))))))

(use-package avy ; jump to thing on screen
  :ensure t
  :bind (("M-;" . avy-goto-char-timer)
	 ("M-g l" . avy-goto-line)
	 :map isearch-mode-map
	 ("C-'" . avy-isearch))
  :custom
  (avy-timeout-seconds 0.5))

(use-package dumb-jump ; jump to definition using rg
  :ensure t
  :functions dumb-jump-xref-activate
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (dumb-jump-force-searcher 'rg)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package org ; org mode
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c L" . org-store-link)
	 :map org-mode-map
	 ("M-g o" . consult-org-heading)
	 ("M-g l" . org-ql-open-link)
	 ("C-M-<return>" . org-insert-todo-heading-respect-content))
  :hook ((org-mode . visual-line-mode)
	 (after-save . mb--auto-update-org-tags))
  :functions (logbook-entry-subheading)
  :defines (org-agenda-custom-commands org-refile-targets)
  :custom
  (org-element-use-cache nil) ; org-journal has a bug https://github.com/bastibe/org-journal/issues/406
  (org-tags-column -77)
  (org-directory (expand-file-name "~/Documents/org"))
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-return-follows-link t)
  (org-babel-load-languages '((emacs-lisp . t)
			      (mermaid . t)))
  :config
  (setopt org-agenda-files (list org-directory))
  (defun mb--auto-update-org-tags ()
    "Automatically update TAGS when saving org files int he org-directory"
    (when (eq major-mode 'org-mode)
      (let ((org-dir (expand-file-name org-directory))
	    (filename (buffer-file-name)))
	(when (and filename (string-prefix-p org-dir filename))
	  (start-process "update-org-tags" nil "ctags" "-Re" "--langmap=org:.org" org-dir)))))

  (let ((weekly '(org-ql-block '(and (not (done))
				     (or (not (deadline))
					 (deadline :before "tomorrow"))
				     (or (not (scheduled))
					 (scheduled :before "tomorrow"))
				     (todo "WEEKLY"))
			       ((org-ql-block-header "= 📅 TODO Weekly 📅 ="))))

	(now  '(org-ql-block '(and (not (done))
				   (or (todo "NOW")
				       (deadline :before "tomorrow"))
				   (or (not (scheduled))
				       (scheduled :before "tomorrow"))
				   (not (todo "WEEKLY")))
			     ((org-ql-block-header "= ⭐️ TODO Now ⭐️ ="))))
	(pending '(org-ql-block '(and (not (done))
				      (or (not (deadline))
					  (deadline :before "tomorrow"))
				      (or (not (scheduled))
					  (scheduled :before "tomorrow"))
				      (not (todo "WEEKLY"))
				      (not (todo "NOW"))
				      (not (todo "DISCUSS"))
				      (todo))
				((org-ql-block-header "= 📌 Pending 📌 ="))))

	(pending-discuss '(org-ql-block '(and (todo "DISCUSS")
					      (not (done))
					      (or (not (deadline))
						  (deadline :before "tomorrow"))
					      (or (not (scheduled))
						  (scheduled :before "tomorrow")))
					((org-ql-block-header "= 🙊 Pending Discussion 🙊 =")))))

    (setq org-agenda-custom-commands `(("d" "Daily agenda"
					(,weekly ,now))
				       ("p" "Planning"
					(,weekly ,now ,pending ,pending-discuss ))))))

(use-package ox-md
  :after org)

(use-package ob-mermaid
  :ensure t
  :after org)

(use-package org-ql
  :requires org
  :ensure t)

(use-package logbook ; the way i org
  :commands logbook-today
  :bind ("M-l" . logbook-today))

(use-package gptel ; chatgpt
  :ensure t
  :commands (gptel-send gptel)
  :bind ("C-c RET" . gptel-send)
  :hook (gptel-mode . visual-line-mode)
  :config
  (setopt
   gptel-api-key (mb-op-read 'gpt-api-key)
   gptel-default-mode 'org-mode
   gptel-model "gpt-4-turbo-preview"))

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

(use-package treesit ; emacs tree sitter
  ;; :disabled
  :defer t

  :mode (("\\.tsx\\'" . tsx-ts-mode)
	 ("\\.ts\\'" . tsx-ts-mode)
	 ("\\.go\\'" . go-ts-mode))

  :custom
  (font-lock-maximum-decoration t)
  (treesit-font-lock-level 4)

  :init
  (setq major-mode-remap-alist
	'((bash-mode . bash-ts-mode)
	  (javascript-mode . tsx-ts-mode)
	  (ruby-mode . ruby-ts-mode)
	  (html-mode . html-ts-mode)
	  (go-mode . go-ts-mode)
	  (shell-script-mode . bash-ts-mode)
	  (typescript-mode . tsx-ts-mode)
	  (json-mode . json-ts-mode)
	  (css-mode . css-ts-mode)
	  (python-mode . python-ts-mode)))

  (setopt treesit-language-source-alist
	  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	    (cmake "https://github.com/uyha/tree-sitter-cmake")
	    (css "https://github.com/tree-sitter/tree-sitter-css")
	    (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
	    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	    (go "https://github.com/tree-sitter/tree-sitter-go")
	    (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	    (html "https://github.com/tree-sitter/tree-sitter-html")
	    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	    (json "https://github.com/tree-sitter/tree-sitter-json")
	    (make "https://github.com/alemuller/tree-sitter-make")
	    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	    (python "https://github.com/tree-sitter/tree-sitter-python")
	    (toml "https://github.com/tree-sitter/tree-sitter-toml")
	    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
	    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
	    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (defun mb-install-all-treesit-langs ()
    "Install all treesit languages."
    (interactive)
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (treesit-install-language-grammar lang))
    (message "All treesit languages installed")))

(use-package markdown-mode ; markdown mode
  :ensure t
  :custom
  (markdown-fontify-code-blocks-natively t))

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
  :ensure t
  :after consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark ; context-based minibuffer actions
  :ensure t
  :defines (embark-indicators)
  :functions
  (embark-prefix-help-command
   which-key--show-keymap
   which-key--hide-popup-ignore-command
   embark--truncate-target)
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
  :ensure t
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
	 ("M-s G" . consult-git-grep)
	 ("M-s g" . consult-ripgrep)
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
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setopt consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  )

(use-package eldoc ; docs in echo area
  :hook (prog-mode . eldoc-mode))

(use-package so-long ; disable fancy things in buffers with giant lines
  :hook (after-init . global-so-long-mode))

(use-package corfu ; in-buffer completion framework (tab complete)
  :ensure t
  :hook ((minibuffer-setup . corfu-enable-in-minibuffer)
	 (go-mode . corfu-enable-auto)
	 (go-ts-mode . corfu-enable-auto))
  :functions (corfu-mode global-corfu-mode)
  :custom
  (tab-always-indent 'complete)
  :init
  (global-corfu-mode)
  (defun corfu-enable-auto ()
    "enable auto complete but only for the current buffer"
    (setq-local corfu-auto t))

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil
		  tab-always-indent 'complete)

      (corfu-mode 1))))

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
	 ("C-r" . consult-history))
  :custom
  (eshell-visual-subcommands '(("rails" "s" "server" "c" "console" "dbconsole")
			       ("docker" "compose"))))

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

(use-package mb-ruby-testing ; choose the right test runner for the current project
  :commands mb-ruby-testing-setup
  :hook (ruby-ts-mode . mb-ruby-testing-setup))

(use-package rspec-mode ; rspec mode
  :ensure t
  :commands rspec-mode)

(use-package minitest ; minitest mode
  :ensure t
  :commands minitest-mode
  :defines minitest-mode-map
  :bind (:map minitest-mode-map
	      ("C-c t t" . minitest-verify-single)
	      ("C-c t a" . minitest-verify)
	      ("C-c t s" . minitest-verify-all)
	      ("C-c t ;" . minitest-rerun)))

(use-package yard-mode ; yard mode for ruby
  :ensure t
  :hook ((ruby-mode ruby-ts-mode) . yard-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   '(fireplace ob-mermaid elfeed-org elfeed mpv trashed dired-subtree diredfl dumb-jump avy org-ql tempel org-journal gptel yaml poly-erb gcmh benchmark-init dired-rainbow dired-narrow dired-hacks nerd-icons-dired dirvish diredful corfu-popupinfo vertico-directory consult direx expreg surround emacs-surround robe-mode robe magit-todos git-link inf-ruby git-timemachine jist feature-mode highlight-defined highlight-defined-mode yaml-mode doom-modeline mini-modeline jsonrpc vertico mmm-mode derived auto-dark eat whole-line-or-region flymake-popon exec-path-from-shell format-all editorconfig s web-mode treesit-auto kind-icon corfu-terminal cape corfu wgrep embark-consult embark marginalia which-key orderless catppuccin-theme)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
(put 'erase-buffer 'disabled nil)
