
(defun make-matrix (rows columns &optional initial)
  "Make a new matrix (rows x columns), composed of initial as value initalisator."
  (let ((result (make-vector rows nil))
        (y 0))
    (while (< y rows)
      (aset result y (make-vector columns initial))
      (setq y (+ y 1)))
    result))

(make-matrix 10 10 'a)

(defun matrix-set (matrix row column value)
  "Given a matrix, set a new value at row - column position with value."
  (aset (aref matrix row) column value))

(setq m (make-matrix 10 10 'a))
(matrix-set  m 2 3 22)
m

(defun matrix-ref (matrix row column)
  "Given a matrix return its elt at row - column position."
  (aref (aref matrix row) column))

;;(matrix-ref m 10 10 )

(defun matrix-columns (matrix)
  "Return the matrix number of columns."
  (length (aref matrix 0)))

;; (defsubst matrix-columns (matrix)
;;   "Return the matrix number of columns."
;;   (length (aref matrix 0)))

(matrix-columns m)
(matrix-columns (make-matrix 5 6))

(defun matrix-rows (matrix)
  "Return the matrix number of rows."
  (length matrix))

(matrix-rows (make-matrix 5 4))

(provide 'matrix)
