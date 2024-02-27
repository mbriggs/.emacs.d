;;; logbook.el --- A simple logbook for org-mode -*- lexical-binding: t; -*-
;;; Author: Matt Briggs <matt@mattbriggs.net>

;;; Commentary:

;; This package provides a simple logbook for org-mode. It is
;; designed to be used as a daily journal, with entries for each day
;; and sections for notes and meetings.

;;; License:

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;;; Code:

(require 'org)
(require 'org-fold)

;; Configuration

(defvar logbook-dir "~/org/logbook"
  "The file to store the logbook in.")

(defvar logbook-file-format "%Y-%m-%b"
  "The format to use for the logbook file name.")

(defvar logbook-date-format "%A, %b-%d-%Y"
  "The date format to use in the logbook.")

(defvar logbook--created-format "%Y-%m-%d"
  "The format to use for the CREATED property.")

;; Interactive

(defun logbook-open (arg)
  "Open or create the current logbook.
If ARG is non-nil, open in current window."
  (interactive "P")
  (logbook-visit (current-time) arg))

(defun logbook-today (arg)
  "Open the logbook for today.
If ARG is non-nil, open in current window."
  (interactive "P")
  (logbook-visit (current-time) arg)
  (logbook-goto-entry (current-time)))

;; Functions

(defun logbook-visit (time &optional current-window)
  "Visit the logbook for TIME.
If CURRENT-WINDOW is non-nil, visit in current window."
  (let ((path (logbook-file-path time)))
    (unless (file-exists-p path)
      (with-temp-buffer
	(insert "#+TITLE: Logbook\n\n")
	(unless (file-exists-p logbook-dir)
	  (make-directory logbook-dir t))
	(write-file path)))
    (if current-window
	(find-file path)
      (logbook--switch-or-find-file path))))

(defun logbook-file-name (time)
  "Return the logbook file name for the given TIME."
  (concat (format-time-string logbook-file-format time) ".org"))

(defun logbook-file-path (time)
  "Return the full path to the logbook file for the given TIME."
  (expand-file-name (logbook-file-name time) logbook-dir))

(defun logbook-entry-p ()
  "Return t if the point is at a logbook entry."
  (let ((created (logbook--read-created)))
    (if (and created
	     (nth 3 created)
	     (nth 4 created)
	     (nth 5 created))
	t
      nil)))

(defun logbook-goto-entry (time)
  "Insert a new entry for TIME in the logbook.
It is inserted in the correct sort order with respect to the
other entries.

Logbook entries are sorted by date, with the most recent entry
at the top of the file."
  (let ((time (logbook--normalize-time time))
	(searching t)
	(requires-insertion t))

    ;; if there are entries, search for the correct place to insert
    (cond
     ((logbook--first-entry) ; there is at least one entry, point is at that heading
      (while searching
	(let ((cmp (if (logbook-entry-p)
		       (logbook--date-cmp time (logbook--read-created))
		     1))) ; treat a non entry heading as a later heading
	  (cond
	   ((= cmp 0) ; found the entry, no need to insert
	    (setq
	     searching nil
	     requires-insertion nil))

	   ((= cmp -1) ; earlier, keep searching if there is more siblings
	    (unless (org-goto-sibling)
	      (setq searching nil)))

	   ((= cmp 1) ; later, go back and insert
	    (setq searching nil))))))


     (t ; no entries
      (goto-char (point-min))
      (cond ; insert either before first heading or at the end of the file

       ((or (org-at-heading-p) ; is the first line a heading?
	    (org-next-visible-heading 1) ; if not, find the first heading
	    (org-at-heading-p)) ; are we on a heading?
	(beginning-of-line)
	(newline)
	(forward-line -1))

       (t ; insert at the end of the file
	(goto-char (point-max))
	(unless (bolp)
	  (newline))))))

    ;; insert the entry
    (when requires-insertion
      (beginning-of-line)
      (newline)
      (forward-line -1)
      (logbook--insert-entry time)
      (org-goto-sibling 1))

    ;; ensure current entry is visible
    (org-fold-show-children)))


(defun logbook-entry-subheading (time heading)
  "Insert or go to a new HEADING as a child of TIME entry."
  (logbook-goto-entry time)
  (logbook-subheading heading))

(defun logbook-subheading (heading)
  "Insert or go to a new HEADING as a child of the current heading."
  (when (not (org-at-heading-p))
    (error "Not at a heading"))
  (org-fold-show-children)
  (let ((found nil)
	(original-point (point)))
    ;; do we have children?
    (when (org-goto-first-child)
      (setq found (logbook--org-heading= heading)) ; does the first child match?
      (while (and (not found)
		  (org-goto-sibling))
	(setq found (logbook--org-heading= heading))))
    (unless found
      (goto-char original-point)
      (org-end-of-subtree)
      (org-insert-heading-respect-content)
      (insert heading))
    found))

;; Helpers

(defun logbook--first-entry ()
  "Find the first logbook entry, and move point to that heading.
Return t if an entry was found, nil otherwise.

If entry is found, point will be on entry. If no entry is found, point
will return to where it was before invoking this function."
  (let ((searching t)
	(found nil)
	(original-point (point)))
    ;; find the first heading
    (goto-char (point-min))
    (org-next-visible-heading 1)

    ;; file has no headings
    (if (not (org-at-heading-p))
	(setq searching nil))

    ;; next sibling until we find an entry or run out of siblings
    (while (and searching
		(org-goto-sibling t)
		(null (logbook-entry-p))))

    ;; we either ran out of siblings or found an entry. In either case, we're done
    (setq found (logbook-entry-p))

    ;; if we didn't find an entry, go back to the original point
    (unless found
      (goto-char original-point))

    ;; return whether we found an entry
    found))

(defun logbook--insert-entry (time)
  "Insert a new entry for TIME."
  (insert (logbook--header time))
  (logbook--write-created time)
  (org-end-of-meta-data)
  (insert
   "** Notes\n"
   "** Meetings\n"))

(defun logbook--header (time)
  "Return the header for the logbook entry for TIME."
  (format-time-string (concat "* " logbook-date-format) (encode-time time)))

(defun logbook--read-created ()
  "Read the CREATED property of the current entry."
  (let ((created (org-entry-get (point) "CREATED")))
    (if (or (null created)
	    (not (org-at-heading-p))
	    (string= created ""))
	nil
      (logbook--normalize-time created))))

(defun logbook--write-created (time)
  "Write TIME to the CREATED property for the current entry."
  (when (not (org-at-heading-p))
    (error "Not at a heading"))
  (org-entry-put (point) "CREATED"
		 (format-time-string logbook--created-format (encode-time time))))

(defun logbook--read-org-heading ()
  "Read an `org-mode` heading."
  (when (not (org-at-heading-p))
    (error "Not at a heading"))
  (let ((heading (substring-no-properties (org-get-heading t t t t))))
    (if (string= heading "")
	(error "No heading at point")
      heading)))

(defun logbook--org-heading= (heading)
  "Return t if the current heading is equal to HEADING."
  (string= heading (logbook--read-org-heading)))

(defun logbook--switch-or-find-file (path)
  "Switch to the buffer for PATH in another window if it's visible.
Otherwise open or switch to it in the current window."
  (if-let ((buf (find-buffer-visiting path))
	   (win (get-buffer-window buf 'visible)))
      ;; If the buffer is visible, switch to its window
      (select-window win)
    ;; If the buffer doesn't exist, open the file in other window
    (find-file-other-window path)))

;; Date functions

(defun logbook--date-cmp (t1 t2)
  "Compare T1 and T2, ignoring hours, minutes, and seconds."
  (let ((d1 (logbook--normalize-time t1))
	(d2 (logbook--normalize-time t2)))
    (cond
     ((time-less-p d1 d2) -1)
     ((time-less-p d2 d1) 1)
     (t 0))))

(defun logbook--normalize-time (time)
  "Normalize TIME to a decoded time list.
TIME may be an encoded time list, a decoded time list, or a string.
TIME will get 0 0 0 for hours, minutes, and seconds."
  (cond
   ;; encoded time
   ((and (listp time) (= 4 (length time)))
    (logbook--truncate-time (decode-time time)))

   ;; decoded time
   ((and (listp time) (= 9 (length time)))
    (logbook--truncate-time time))

   ;; time string
   ((stringp time)
    (logbook--truncate-time
     (parse-time-string time)))

   (t (error "Invalid time"))))

(defun logbook--truncate-time (time)
  "Trunce the time part of TIME, setting hours, minutes, and seconds to 0.
TIME is a decoded time list."
  (let ((tm (copy-sequence time)))
    (setf (nth 0 tm) 0)
    (setf (nth 1 tm) 0)
    (setf (nth 2 tm) 0)
    tm))

(provide 'logbook)
;;; logbook.el ends here
