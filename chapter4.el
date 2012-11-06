
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

;;WRITETIMESTAMP(())

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

;;WRITETIMESTAMP((Mon Nov  5 22:42:11 2012 848372000))

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

;;WRITETIMESTAMP((Mon Nov  5 22:42:11 2012 848372000))

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

;;WRITETIMESTAMP((Mon Nov  5 22:42:11 2012 848372000))

;; THIRD section. Modifying the timestamp on first change in the buffer.

(defvar modify-format "%c %N"
  "*Format in use when overwriting the modifytimestamps. (c.f. 'format-time-string).")

(defvar modify-prefix  ";;MODIFYTIMESTAMP(("
  "*Used prefix for a modifytimestamp.")

(defvar modify-suffix "))"
  "*Used suffix for a modifytimestamp.")

(defun update-modifystamps ()
  "Find modifytimestamps and update them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (let ((time-stamp (format-time-string modify-format (current-time))))
          (while (re-search-forward (concat
                                     (concat "^" (regexp-quote modify-prefix))
                                     "\\(.*\\)"
                                     (concat (regexp-quote modify-suffix) "$"))
                                    nil t)
            (replace-match time-stamp t t nil 1))))))
  nil)

;(make-local-variable 'first-change-hook)
(add-hook 'first-change-hook 'update-modifystamps nil t)
(add-hook 'first-change-hook 'ns-unselect-line nil t)

;;MODIFYTIMESTAMP((Mon Nov  5 23:37:17 2012 300684000))

;; Second approach. for store the last modifytimestamps before saving
;; the buffer.
(add-hook 'local-write-file-hooks 'update-writetimestamps-on-modified-buf nil t)

(defun update-writetimestamps-on-modified-buf ()
  "If the current buffer has been modified then update write timestamps."
  (if (buffer-modified-p)
      (update-modifystamps)))

;; Thirst approach. Only modify the write time stamps when saving the
;; file (and before saving it), and with the last modification
;; timestamp.

(defvar last-modification-timestamp nil
  "Time when the last modification occured.")
(make-variable-buffer-local 'last-modification-timestamp)

(add-hook 'after-change-functions 'store-last-modification-timestamp nil t)
 
(defun store-last-modification-timestamp (beg end length)
  "Store the current time into last-modification-timestamp."
  (setq last-modification-timestamp (current-time)))

(defun update-modifystamps ()
  "Find modifytimestamps and update them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (let ((time-stamp (format-time-string modify-format last-modification-timestamp)))
          (while (re-search-forward (concat
                                     (concat "^" (regexp-quote modify-prefix))
                                     "\\(.*\\)"
                                     (concat (regexp-quote modify-suffix) "$"))
                                    nil t)
            (replace-match time-stamp t t nil 1))))))
  nil)
 
;; Last approach in the book to cover a subtle bug is not necessary in
;; recent version of emacs.
;; From (describe-variable 'after-change-functions):
;; "Buffer changes made while executing the `after-change-functions'
;; don't call any before-change or after-change functions.
;; That's because `inhibit-modification-hooks' is temporarily set
;; non-nil."
;; That said, the author (Bob Glickstein) proposes to capture the
;; timestamp value when invoking the 'update-modifytimestamps
;; function. Let's do it.

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

;;MODIFYTIMESTAMP((Mon Nov  5 23:37:17 2012 300684000))

