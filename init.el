;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my v3 Emacs configuration, and the first time I've used holy Emacs.
;; Using as much of the built in functionality as possible. Naviagation of this
;; file is intended to happen via consult-outline (M-g o).

;;; Code:

(use-package package ; package setup
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(use-package emacs ; core configuration
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
   )

  ;; theme
  (set-frame-font "JetBrains Mono 14" nil t)

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
  )

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

(use-package magit ; git porcelain
  :ensure t
  :bind (("C-x g" . magit-status)))

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

(use-package flymake ; on the fly syntax checking
  :hook (prog-mode . flymake-mode))

;; TODO: swap this as soon as someone makes a childframe impl for flymake
(use-package flymake-popon ; show flymake errors in a childframe
  :ensure t
  :hook (flymake-mode . flymake-popon-mode))

(use-package eglot ; language server protocol
  :requires jsonrpc
  :hook
  ((ruby-mode . eglot-ensure)
   (ruby-ts-mode . eglot-ensure)
   (yaml-ts-mode . eglot-ensure)
   (typescript-ts-mode .eglot-ensure)
   (js-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (web-mode . eglot-ensure)
   (sh-mode . eglot-ensure)
   (bash-ts-mode . eglot-ensure)
   (html-mode . eglot-ensure)
   (css-mode . eglot-ensure)
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


;; Completion -- minibuffer, intellisense, etc
(load-file (expand-file-name "completion.el" user-emacs-directory))

;; Global key bindings
(load-file (expand-file-name "binding.el" user-emacs-directory))

;; Modes for languages not bundled
(load-file (expand-file-name "lang.el" user-emacs-directory))

;; Modeline
(load-file (expand-file-name "lang.el" user-emacs-directory))

   ;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(jsonrpc vertico mmm-mode derived auto-dark eat whole-line-or-region flymake-popon exec-path-from-shell format-all editorconfig s web-mode treesit-auto kind-icon corfu-terminal cape corfu wgrep embark-consult embark marginalia which-key orderless catppuccin-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
