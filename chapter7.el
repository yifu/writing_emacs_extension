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

(defun before-second-word-p (pos)
  "Is pos begore the second word in its line?"
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward "^ ")
    (skip-chars-forward " ")
    (< pos (point))))

;; (defun myrefill (beg end len)
;;   "After a change, myrefill the paragrap"
;;   (let ((left (if (or (equal 0 len) (not (before-second-word-p beg)))
;;                   beg ;; In case of an insertion return the start of
;;                 ;; the insertion.
;;                 (save-excursion
;;                   (max
;;                    (progn
;;                      (goto-char beg)
;;                      (backward-paragraph 1)
;;                      (point))
;;                    (progn
;;                      (goto-char beg) ;; eol of the previous line.
;;                      (beginning-of-line 0)
;;                      (point)))))))
;;     (save-excursion
;;       (message "Myrefill start %d beg %d end %d len %d, start == beg? %S " left beg end len (equal beg left))
;;       (fill-region left end nil nil t))))

(defun myrefill (start end len)
  "Refill a paragraph at point."
  (let ((left (if (zerop len)
                  start
                (save-excursion
                  (goto-char start)
                  (forward-line -1)
                  (beginning-of-line)
                  (point)))))
    (fill-region left end nil nil t)))
