;; Treesitter

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Other

(use-package markdown-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.html$" . web-mode)
	 ("\\.erb$" . web-mode)))
