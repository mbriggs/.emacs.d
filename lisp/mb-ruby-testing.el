;;; mb-ruby-testing.el --- Ruby testing setup
;;; Commentary:

;; mb-ruby-testing-setup will setup either rspec or minitest in a ruby project, based
;; on the contents of the Gemfile. If rspec is found, rspec-mode will be enabled, otherwise
;; minitest-mode will be enabled (the default).

;; The expected usage is to add a hook to the ruby-mode-hook, like so:
;; (add-hook 'ruby-ts-mode-hook 'mb-ruby-testing-setup)

;;; Dependencies:

;; This package depends on the following packages:
;; - rspec-mode
;; - minitest-mode

;;; Code:

(declare-function rspec-mode "rspec-mode")
(declare-function minitest-mode "minitest-mode")

;;;###autoload
(defun mb-ruby-testing-setup ()
  "Setup either rspec or minitest in a ruby project.
If no gemfile is found, nothing will happen."
  (interactive)
  (when-let ((gemfile-path (mb-ruby-testing-gemfile-path)))
    (if (mb-gemfile-contains-gem-p gemfile-path "rspec")
	(rspec-mode)
      (progn
	(minitest-mode)
	(setq-local
	 minitest-use-spring (mb-gemfile-contains-gem-p gemfile-path "spring")
	 minitest-use-rails (mb-gemfile-contains-gem-p gemfile-path "rails"))))))

(defun mb-ruby-testing-gemfile-path ()
  "Search for a Gemfile and return its path."
  (let ((dir (locate-dominating-file (or (buffer-file-name) default-directory) "Gemfile")))
    (when dir
      (expand-file-name "Gemfile" dir))))

(defun mb-gemfile-contains-gem-p (gemfile-path gem)
  "Check if GEMFILE-PATH contains GEM."
  (mb-file-contains-regexp-p gemfile-path (format "^[[:space:]]*gem ['\"]%s['\"]" gem)))

(defun mb-file-contains-regexp-p (file-path regexp)
  "Check if FILE-PATH contains a string that matches REGEXP."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (if (re-search-forward regexp nil t)
	t
      nil)))

(provide 'mb-ruby-testing)
;;; mb-ruby-testing.el ends here
