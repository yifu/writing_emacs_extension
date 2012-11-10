;; Chapter 8. Evaluation and error recovery.
(defmacro limited-save-excursion (&rest body)
  "Behaves like save-excursion, but only restore the point."
  (let ((orig-point-symbol (make-symbol "orig-point")))
    `(let* ((,orig-point-symbol (point-marker)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,orig-point-symbol)))))

(provide 'limited)

(myrefill (point) (+ (point) 3) 0)
