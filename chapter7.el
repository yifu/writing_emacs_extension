;; Chapter 7. Minor Mode.

;; Section 1, 2, 3 and 4. Paragraph filling.

(defvar myrefill-mode nil
  "Mode variable for the refill minor mode.")
(make-variable-buffer-local 'myrefill-mode)

(defun myrefill-mode (&optional arg)
  "Turn on/off the myrefill-mode minor mode."
  (interactive "P")
  (setq myrefill-mode (if (null arg)
                        (not myrefill-mode)
                      (> (prefix-numeric-value arg) 0)))
  (if myrefill-mode
      (add-hook 'after-change-functions 'myrefill nil t)
    (remove-hook 'after-change-functions 'myrefill t)))

(if (null (assq 'myrefill-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(myrefill-mode " Myrefill") minor-mode-alist)))

(defun myrefill (beg end len)
  "After a change, myrefill the paragrap"
  (message "Myrefill fun.")
  (let ((left (if (equal 0 len)
                  beg ;; In case of an insertion return the start of
                ;; the insertion.
                (save-excursion ;; In case of a removing return the
                  ;; eol of the previous line.
                  (goto-char beg)
                  (beginning-of-line 0)
                  (point)))))
    (save-excursion
      (message "Myrefill start %d" left)
      (fill-region left end nil nil t))))
