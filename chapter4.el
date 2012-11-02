
;; First section. Play with string insertion, and timestamp displaying,

;;(current-time-string)
;;"Fri Nov  2 22:13:16 2012"

(defun insert-current-time ()
  "Insert the current time string, at point"
  (interactive "*")
  (insert (current-time-string)))

;;(insert-current-time)
;;Fri Nov  2 22:19:16 2012

;;(format-time-string "%l.%M %p" (current-time))
;;"10.24 PM"

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

;;(insert-time)22:49:05
;;(insert-date)11/02/12
;;(stringp insert-time-format)
;;(stringp insert-date-format)

;; Second section. Plating with write-file-* hooks, Overwriting writetimestamps.
(add-hook 'local-write-file-hooks 'update-writetimestamps)

;;WRITETIMESTAMP((23:58:30))

(defun update-writetimestamps ()
  "Find writetimestamps and update them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (while (search-forward (concat "WRITETIMESTAMP" "((") nil t)
          (let ((start (point)))
            (search-forward "))")
            (delete-region start (- (point) 2))
            (goto-char start)
            (insert-time))))))
  nil)

;;WRITETIMESTAMP((23:58:30))

