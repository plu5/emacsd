;;; org-simple-expiry.el -- Simple creation dates for org headings
;;; Commentary:
;; Only includes creation date related stuff from Bastien Guerry’s
;; org-expiry.  Does not set hooks or advices.  Just bind
;; `org-expiry-insert-created' to be able to easily add creation date
;; to entries but not be forced to do so.
;;; Code:
(defvar org-expiry-inactive-timestamps nil
  "Insert inactive timestamps for created/expired properties.")

(defvar org-expiry-created-property-name "CREATED"
  "The name of the property for setting the creation date.")

(defvar org-expiry-created-date "+0d"
  "The default creation date.
The default value of this variable (\"+0d\") means that entries
without a creation date will be handled as if they were created
today.

If the creation date cannot be retrieved from the entry or the
subtree above, the expiry process will compare the expiry delay
with this date.  This can be either an ISO date or a relative
time specification.  See `org-read-date' for details on relative
time specifications.")
  
(defun org-expiry-insert-created (&optional arg) ; Bastien Guerry
  "Insert or update a property with the creation date.
If ARG, always update it.  With one `C-u' prefix, silently update
to today's date.  With two `C-u' prefixes, prompt the user for to
update the date."
  (interactive "P")
  (let* ((d (org-entry-get (point) org-expiry-created-property-name))
	 d-time d-hour timestr)
    (when (or (null d) arg)
      ;; update if no date or non-nil prefix argument
      ;; FIXME Use `org-time-string-to-time'
      (setq d-time (if d (org-time-string-to-time d)
		     (current-time)))
      (setq d-hour (format-time-string "%H:%M" d-time))
      (setq timestr
	    ;; two C-u prefixes will call org-read-date
	    (if (equal arg '(16))
		(concat "<" (org-read-date
			     nil nil nil nil d-time d-hour) ">")
	      (format-time-string (org-time-stamp-format t))))
      ;; maybe transform to inactive timestamp
      (if org-expiry-inactive-timestamps
	  (setq timestr (concat "[" (substring timestr 1 -1) "]")))
      (save-excursion
	(org-entry-put
	 (point) org-expiry-created-property-name timestr)))))

(provide 'org-simple-expiry)
;;; org-simple-expiry.el ends here
