
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

(defvar write-format "%c %N"
  "*Format in use when overwriting the writetimestamps. (c.f. 'format-time-string).")

(defvar write-prefix  ";;WRITETIMESTAMP(("
  "*Used prefix for a writetimestamp.")

(defvar write-suffix "))"
  "*Used suffix for a writetimestamp.")

;;WRITETIMESTAMP((Mon Nov  5 22:07:40 2012 527921000))

(defun update-writetimestamps ()
  "Find writetimestamps and update them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (while (search-forward write-prefix nil t)
          (let ((start (point)))
            (search-forward write-suffix)
            (delete-region start (match-beginning 0))
            (goto-char start)
            (insert (format-time-string
                     write-format
                     (current-time))))))))
  nil)

;;WRITETIMESTAMP((Mon Nov  5 22:07:40 2012 527921000))

;; Second version, with regex.
(defun update-writetimestamps ()
  "Find writetimestamps and update them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (while (re-search-forward (concat "^" (regexp-quote write-prefix)) nil t)
          (let ((start (point))
                (eol-pos (save-excursion
                           (end-of-line)
                           (point))))
            (if (re-search-forward (concat (regexp-quote write-suffix) "$") eol-pos t)
                (progn
                  (delete-region start (match-beginning 0))
                  (goto-char start)
                  (insert (format-time-string
                           write-format
                           (current-time))))))))))
  nil)

;;WRITETIMESTAMP((Mon Nov  5 22:07:40 2012 527921000))

;; Third version, using all the regex power.
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

;;WRITETIMESTAMP((Mon Nov  5 22:07:40 2012 527921000))
