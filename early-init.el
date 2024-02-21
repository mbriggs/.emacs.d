;;; early-init.el --- Early initialization. -*- lexical-binding: t; -*-
;;; Commentary:

;; Init code that runs before the package system and GUI is initialized.

;;; Code:

(setq frame-resize-pixelwise t)
(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway
(setq default-frame-alist '(
			    ;; You can turn off scroll bars by uncommenting these lines:
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
			    (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))
;;; early-init.el ends here
(provide 'early-init)
