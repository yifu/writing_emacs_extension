
(defun make-crossword (size)
  "Make a crossword grid with SIZE rows and columns."
  (if (zerop (% size 2))
      (error "make-crossword: size must be odd."))
  (if (< size 3)
      (error "make-crossword: size must be 3 or greater."))
  (make-matrix size size nil))

(make-crossword 0) 
(make-crossword 1) 
(make-crossword 2) 
(make-crossword 3) 
(make-crossword 4) 
(make-crossword 11)

(defsubst crossword-size (crossword)
  "Return the size of the CROSSWORD."
  (matrix-rows crossword))

(crossword-size (make-crossword 3) )
(crossword-size (make-crossword 11))

(defsubst crossword-ref (crossword row column)
  "Get the element of CROSSWORD at ROW and COLUMN,"
  (matrix-ref crossword row column))

(crossword-ref (make-crossword 3) 0 2)

(defsubst crossword--set (crossword row column value)
  "Set the VALUE in CROSSWORD at position ROW - COLUMN."
  (matrix-set crossword row column value))

(setq c (make-crossword 11))
(crossword--set c 2 3 'charlie)
c

(defun crossword-cousin-position (crossword row column)
  "Return the cousin position of ROW - COLUMN in CROSSWORD."
  (let ((size (crossword-size crossword)))
    (cons (- size row 1) (- size column 1))))

(crossword-cousin-position (make-crossword 21) 0 0)
(crossword-cousin-position (make-crossword 21) 0 1)
(crossword-cousin-position (make-crossword 21) 10 10)
(equal '(4 . 5)
       (let ((cousin-pos (crossword-cousin-position c 4 5)))
         (crossword-cousin-position c (car cousin-pos) (cdr cousin-pos))))

(defun crossword-cousin-ref (crossword row column)
  "Return the value for the cousin of the block at ROW - COLUMN in CROSSWORD,"
  (let ((cousin-position (crossword-cousin-position crossword row column)))
    (crossword-ref crossword (car cousin-position) (cdr cousin-position))))

(crossword-cousin-position c 2 3) ;;(8 . 7)
(crossword-cousin-ref c 8 7) ;;charlie

(defun crossword-cousin--set (crossword row column value)
  "Set the VALUE in CROSSWORD at cousin position of ROW - VALUE."
  (let ((cousin-position (crossword-cousin-position crossword row column)))
    (crossword--set crossword (car cousin-position) (cdr cousin-position) value)))

(crossword-cousin--set c 8 7 'toto)
(crossword-ref c 2 3) ;;toto
