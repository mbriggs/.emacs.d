;;; mb-op.el --- 1Password integration for Emacs

;;; Commentary:

;; This package provides a simple interface to 1Password for Emacs. The idea is
;; to register references to 1Password items by role, and then be able to read
;; them from Emacs.

;;; Example usage:

;; (mb-op-register-reference 'gist-token "op://Personal/lrian52eh4tkos2lgabjmkh6ve/credential")
;;
;; (use-package mb-op ; realistically this would be in your init.el
;;  :commands mb-op-read
;;  :config
;;  (mb-op-register-reference 'gist-token "op://Personal/lrian52eh4tkos2lgabjmkh6ve/credential"))
;;
;; (mb-op-read 'gist-token)

;;; Code:

(defvar mb-op-references-alist '()
  "List of one password references keyed by role e.g. `gist-token`.")

(defun mb-op-register-reference (role reference)
  "Register a reference for a given role.
ROLE is a symbol, eg `gist-token`.
REFERENCE is a 1password reference string for a field, eg
          `op://Personal/lrian52eh4tkos2lgabjmkh6ve/credential`."
  (add-to-list 'mb-op-references-alist (cons role reference)))

(defun mb-op-get-reference (role)
  "Get a reference for a given role.
ROLE is a symbol, eg `gist-token`."
  (cdr (assoc role mb-op-references-alist)))

;;;###autoload
(defun mb-op-read (role)
  "Run `op read <reference>` for a given role.
ROLE is a symbol, eg `gist-token`"
  (interactive "SRole: ")
  (let ((reference (mb-op-get-reference role)))
    (when (not reference)
      (error (format "No reference found for role %s" role)))
    (string-trim-right
     (shell-command-to-string (format "op read \"%s\"" reference)))))


(provide 'mb-op)
;;; mb-op.el ends here
