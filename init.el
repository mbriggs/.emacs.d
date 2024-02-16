;; Package init
(require 'package)
(require 'use-package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;;; Mac uses cmd as meta
(when (eq system-type 'darwin)
  (setq
   mac-command-modifier 'meta
   mac-option-modifier 'super
   mac-right-command-modifier 'control))

;;; UI

;; stop the beeping
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
;;
;; (setopt indent-tabs-mode nil)
;; (setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;; font
(set-frame-font "JetBrains Mono 14" nil t)

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'frappe)
  (catppuccin-reload))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; Basic settings

(setopt inhibit-splash-screen t)

;; auto reload file
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Repeat common things
(repeat-mode)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun mb--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(setopt make-backup-file-name-function 'mb--backup-file-name)


;;; Git

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter
  :ensure t
  :requires (git-gutter-fringe fringe-helper)
  :config
  (require 'git-gutter-fringe)
  (require 'git-gutter)
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

;;; PATH should match shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;; Editable grep buffers
(use-package wgrep
  :ensure t)


;;; Tab bar
;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;;; LSP
(use-package eglot
  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
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
  :hook (prog-mode . copilot-mode)
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
  :ensure t)

;;; editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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
   '(exec-path-from-shell format-all editorconfig s web-mode treesit-auto kind-icon corfu-terminal cape corfu wgrep embark-consult embark marginalia which-key orderless vertico catppuccin-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
