
;; Section one.

(defun insert-current-time ()
  "Insert the current time string, at point"
  (interactive "*")
  (insert (current-time-string)))

(defvar insert-time-format "%X"
  "*Format for \\[insert-time] (c.f. 'format-time-string').")

(defvar insert-date-format "%x"
  "*Format for \\[insert-date] (c.f. 'format-time-string').")

(defun insert-time ()
  "Insert time at point, using the insert-time-format."
  (interactive "*")
  (insert
   (format-time-string
    insert-time-format
    (current-time))))

(defun insert-date ()
  "Insert the date at point, using the insert-date-format"
  (interactive "*")
  (insert
   (format-time-string
    insert-date-format
    (current-time))))

(defvar write-format "%c %N"
  "*Format in use when overwriting the writetimestamps. (c.f. 'format-time-string).")

(defvar write-prefix  ";;WRITETIMESTAMP(("
  "*Used prefix for a writetimestamp.")

(defvar write-suffix "))"
  "*Used suffix for a writetimestamp.")

(defun update-writetimestamps ()
  "Find writetimestamps and update them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (let ((time-stamp (format-time-string write-format (current-time))))
          (while (re-search-forward (concat
                                     (concat "^" (regexp-quote write-prefix))
                                     "\\(.*\\)"
                                     (concat (regexp-quote write-suffix) "$"))
                                    nil t)
            (replace-match time-stamp t t nil 1))))))
  nil)

(defvar modify-format "%c %N"
  "*Format in use when overwriting the modifytimestamps. (c.f. 'format-time-string).")

(defvar modify-prefix  ";;MODIFYTIMESTAMP(("
  "*Used prefix for a modifytimestamp.")

(defvar modify-suffix "))"
  "*Used suffix for a modifytimestamp.")

;;MODIFYTIMESTAMP((Tue Nov  6 22:12:52 2012 681071000))

(defvar last-modification-timestamp nil
  "Time when the last modification occured.")

(defun store-last-modification-timestamp (beg end length)
  "Store the current time into last-modification-timestamp."
  (setq last-modification-timestamp (current-time)))
 
(defun update-modifystamps (time-stamp)
  "Find modifytimestamps and update them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (let ((time-stamp (format-time-string modify-format time-stamp)))
          (while (re-search-forward (concat
                                     (concat "^" (regexp-quote modify-prefix))
                                     "\\(.*\\)"
                                     (concat (regexp-quote modify-suffix) "$"))
                                    nil t)
            (replace-match time-stamp t t nil 1))))))
  nil)

(defun update-writetimestamps-on-modified-buf ()
  "If the current buffer has been modified then update write timestamps."
  (if last-modification-timestamp
      (update-modifystamps last-modification-timestamp)))

(provide 'timestamp)

;; Local Variables:
;; insert-time-format: "%X"
;; insert-date-format: "%x"
;; write-format: "%c %N"
;; write-prefix:  ";;WRITETIMESTAMP(("
;; write-suffix: "))"
;; modify-format: "%c %N"
;; modify-prefix:  ";;MODIFYTIMESTAMP(("
;; modify-suffix: "))"
;; eval: (add-hook 'local-write-file-hooks 'update-writetimestamps-on-modified-buf nil t)
;; last-modification-timestamp: nil
;; eval: (add-hook 'after-change-functions 'store-last-modification-timestamp nil t)
;; End:


