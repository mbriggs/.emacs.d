;;; early-init.el --- Early initialization. -*- lexical-binding: t; -*-
;;; Commentary:

;; Init code that runs before the package system and GUI is initialized.

;;; Code:

;; PERF: Garbage collection is a big contributor to startup times. This fends it
;;   off, but will be reset later by `gcmh-mode' (or in doom-cli.el, if in a
;;   noninteractive session). Not resetting it later causes stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

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
