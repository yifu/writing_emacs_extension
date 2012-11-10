;; Chapter 8. Evaluation and error recovery.

;; First macro attempt.
(defmacro limited-save-excursion (&rest body)
  "Behaves like save-excursion, but only restore the point."
  (append '(let ((orig-point (point))))
          body
          '((goto-char orig-point))))

(macroexpand '(limited-save-excursion (beginning-of-line 0)))
;; (let ((orig-point (point)))
;;   (beginning-of-line 0)
;;   (goto-char orig-point))

(macroexpand '(limited-save-excursion (beginning-of-line 0) (point)))
;;(let ((orig-point (point))) (beginning-of-line 0) (point) (goto-char
;orig-point))


;; Second macro attempt. Backquoting.
(defmacro limited-save-excursion (&rest body)
  "Behaves like save-excursion, but only restore the point."
  `(let ((orig-point (point)))
     ,@body
     (goto-char orig-point)))

(macroexpand '(limited-save-excursion (beginning-of-line 0)))
;(let ((orig-point (point))) (beginning-of-line 0) (goto-char orig-point))

(macroexpand '(limited-save-excursion (beginning-of-line 0) (point)))
;(let ((orig-point (point))) (beginning-of-line 0) (point) (goto-char
;orig-point))

;; Third attempt. Working out the return value.
(defmacro limited-save-excursion (&rest body)
    "Behaves like save-excursion, but only restore the point."
    `(let* ((orig-point (point))
            (result (progn ,@body)))
       (goto-char orig-point)
       result))


(limited-save-excursion (beginning-of-line 0))

(limited-save-excursion (beginning-of-line 0) (point))


;; Fourth attempt. Dealing with symbol conflicts (a.k.a hygienic macros).
(defmacro limited-save-excursion (&rest body)
    "Behaves like save-excursion, but only restore the point."
    (let ((orig-point-symbol (make-symbol "orig-point")))
      `(let* ((,orig-point-symbol (point))
              (result (progn ,@body)))
         (goto-char ,orig-point-symbol)
         result)))

(limited-save-excursion (beginning-of-line 0))

(limited-save-excursion (beginning-of-line 0) (point))
;; (let*
;;     ((orig-point
;;       (point))
;;      (result
;;       (progn
;;         (beginning-of-line 0)
;;         (point))))
;;   (goto-char orig-point)
;;   result)
;1902

;; Fifth attempt. Failing gracefully.
(defmacro limited-save-excursion (&rest body)
  "Behaves like save-excursion, but only restore the point."
  (let ((orig-point-symbol (make-symbol "orig-point")))
    `(let* ((,orig-point-symbol (point)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,orig-point-symbol)))))


(limited-save-excursion (beginning-of-line 0))
;; (let*
;;     ((orig-point
;;       (point)))
;;   (unwind-protect
;;       (progn
;;         (beginning-of-line 0))
;;     (goto-char orig-point)))

(limited-save-excursion (beginning-of-line 0) (point))
;; (let*
;;     ((orig-point
;;       (point)))
;;   (unwind-protect
;;       (progn
;;         (beginning-of-line 0)
;;         (point))
;;     (goto-char orig-point)))

;; Sixth and last attempt. Using marker (relative position).
(defmacro limited-save-excursion (&rest body)
  "Behaves like save-excursion, but only restore the point."
  (let ((orig-point-symbol (make-symbol "orig-point")))
    `(let* ((,orig-point-symbol (point-marker)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,orig-point-symbol)))))


(limited-save-excursion (beginning-of-line 0))
;; (let*
;;     ((orig-point
;;       (point-marker)))
;;   (unwind-protect
;;       (progn
;;         (beginning-of-line 0))
;;     (goto-char orig-point)))

(limited-save-excursion (beginning-of-line 0) (point))
;; (let*
;;     ((orig-point
;;       (point-marker)))
;;   (unwind-protect
;;       (progn
;;         (beginning-of-line 0)
;;         (point))
;;     (goto-char orig-point)))

(provide 'limited)
