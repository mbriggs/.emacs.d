;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; This is my v3 Emacs configuration, and the first time I've used holy Emacs.

;;; Code:

;; Package init
(require 'package)
(require 'use-package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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

(use-package catppuccin-theme
  :ensure t
  :defines catppuccin-flavor
  :functions catppuccin-reload
  :config
  (setq catppuccin-flavor 'frappe)
  (catppuccin-reload))


;; misc ui settings
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling
(repeat-mode) ; Repeat common commands with a single key press
(fset 'yes-or-no-p 'y-or-n-p)                          ; y/n instead of yes/no
(show-paren-mode)                                      ; Highlight matching parens


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


;; Compile init.el on save

(defun mb--compile-init-on-save ()
  "Compile init.el on save."
  (when (equal (file-name-nondirectory (buffer-file-name)) "init.el")
    (message "Byte-compiling init.el (from specific hook in mb--compile-init-on-save)...")
    ;; Byte-compile init.el. This should trigger native-compilation automatically if enabled.
    (byte-compile-file (buffer-file-name))))

(defun mb-add-compile-init-on-save-hook ()
  "Add a hook to compile init.el on save."
  (add-hook 'after-save-hook #'mb--compile-init-on-save nil 'local))

(add-hook 'emacs-lisp-mode-hook #'mb-add-compile-init-on-save-hook)

;; Enable dired-x when dired loads
(with-eval-after-load 'dired
  (require 'dired-x))

;; Commands work on the current line if no region is selected
(use-package whole-line-or-region
  :ensure t
  :functions whole-line-or-region-global-mode
  :config
  (whole-line-or-region-global-mode))

;; Enable the tab-bar
(use-package tab-bar
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

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

(use-package recentf
  :init
  (setopt recentf-max-saved-items 100)
  (setopt recentf-auto-cleanup 'never)
  :config
  (recentf-mode))

;; Show possible keys after a short delay
(use-package which-key
  :ensure t
  :functions which-key-mode
  :config
  (which-key-mode))

;; Show an inspirational quote at startup
(use-package mb-fortune
  :load-path "lisp"
  :functions mb-fortune
  :config
  (setq initial-scratch-message (concat ";; " (mb-fortune) "\n\n")))


;; Git

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter
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

;; PATH should match shell
(use-package exec-path-from-shell
  :ensure t
  :functions exec-path-from-shell-initialize
  :config
  (exec-path-from-shell-initialize))

;; Editable grep buffers
(use-package wgrep
  :ensure t)

;;; LSP
(use-package flymake
  :hook (prog-mode . flymake-mode))

;; TODO: swap this as soon as someone makes a childframe impl for flymake
(use-package flymake-popon
  :ensure t
  :hook (flymake-mode . flymake-popon-mode))

(use-package eglot
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

(use-package format-all
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

;;; Copilot
(use-package copilot
  :after (editorconfig dash s jsonrpc)
  :load-path ".copilot"
  :hook ((prog-mode . copilot-mode)
	 (copilot-mode . (lambda ()
			   (setq-local copilot--indent-warning-printed-p t))))
  :defines (copilot-indentation-alist copilot-completion-map)
  :bind (:map copilot-completion-map
	      ("M-;" . copilot-accept-completion))
  :config
  (add-to-list 'copilot-indentation-alist
	       '(lisp-interaction-mode . (lambda () (current-column)))
	       '(emacs-lisp-mode . (lambda () (current-column)))))

(use-package dash
  :ensure t)

(use-package s
  :ensure t)

(use-package jsonrpc
  :pin gnu
  :ensure t
  :functions jsonrpc--log-event
  :config
  (fset #'jsonrpc--log-event #'ignore))  ; massive perf boost---don't log every event)

   ;;; editorconfig
(use-package editorconfig
  :ensure t
  :functions editorconfig-mode
  :config
  (editorconfig-mode))

   ;;;;; External files

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
   '(whole-line-or-region flymake-popon exec-path-from-shell format-all editorconfig s web-mode treesit-auto kind-icon corfu-terminal cape corfu wgrep embark-consult embark marginalia which-key orderless vertico catppuccin-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
