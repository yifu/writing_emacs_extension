;; List processing.

;; Section 1. Simple View.
(car '()) ;;nil
(cdr '()) ;;nil

(car '(nil)) ;;nil
(cdr '(nil)) ;;nil

(list 'a "b" 7) ;;(a "b" 7)
(list '(x y z) 3) ;;((x y z) 3)

(cons 'a '(2 3 4) ) ;;(a 2 3 4)
(cons '(a b) '(c d)) ;;((a b) c d)

(setq x '(a b c)) ;;(a b c)
(setq y (cons 17 x)) ;;(17 a b c)

(append '(a b) '(c d)) ;;(a b c d)
(append '(a (b c) d) '(e (f))) ;;(a (b c) d e (f))

(reverse '(a b c)) ;;(c b a)
(reverse '(1 2 (3 4) 5 6)) ;;(6 5 (3 4) 2 1)

;; Section 2. List details.
(car (cons 'a 'b)) ;;a
(cdr (cons 'a 'b)) ;;b

(cons 'a 'b) ;;(a . b)
(cons '(1 2) 3) ;;((1 2) . 3)

(cons 'a nil) ;;(a)
(cons 'a (cons 'b 'c)) ;;(a b . c)

(cons 'a (cons 'b nil)) ;;(a b)

(setq x '(a b c)) ;;(a b c)
(setq y (cdr x)) ;;(b c)

(consp (cons 'a 'b)) ;;t
(consp (cons 'b (cons 'c nil))) ;;t
(consp nil) ;;nil
(consp 'a) ;;nil

(atom (cons 'a 'b)) ;;nil
(atom (cons 'b (cons 'c nil))) ;;nil
(atom nil) ;;t
(atom 'a) ;;t
(atom "test") ;;t
(atom 3) ;;t

(listp (cons 'a 'b)) ;;t
(listp (cons 'b (cons 'c nil))) ;;t
(listp nil) ;;t
(listp 'a) ;;nil
(listp "test") ;;nil
(listp 3) ;;nil

(null (cons 'a 'b)) ;;nil
(null (cons 'b (cons 'c nil))) ;;nil
(null nil) ;;t
(null 'a) ;;nil
(null "test") ;;nil
(null 3) ;;nil

(car nil) ;;nil
(cdr nil) ;;nil

;; Section 3. Recursive list functions.
(defun flatten (lst)
  (if (null lst)
      nil
    (if (listp (car lst))
        (append (flatten (car lst))
                (flatten (cdr lst)))
      (cons (car lst)
            (flatten (cdr lst))))))

(flatten '(a (b c d) e)) ;;(a b c d e)
(flatten '(a ((b) c) d)) ;;(a b c d)
(flatten '()) ;;nil
(flatten '(a)) ;;(a)
;;(flatten 'a)

;; Section 4. Iterative list functions.
(defun count-syms (lst)
  (if (null lst)
      0
    (if (symbolp (car lst))
        (+ 1 (count-syms (cdr lst)))
      (count-syms (cdr lst)))))

(defun count-syms (lst)
  (let ((result 0))
    (while lst
      (if (symbolp (car lst))
          (setq result (+ 1 result)))
      (setq lst (cdr lst)))
    result))

(count-syms '()) ;;0
(count-syms '(a)) ;;1
(count-syms '(a b)) ;;2
(count-syms '((a b) c d e f)) ;;4

;; Section 5. Other useful list functions,
(length nil) ;;0
(length '(a b c)) ;;3
(length '((x y z))) ;;1
;;(length '(a b . c))

(nthcdr 3 '(a b c d e)) ;;(d e)
(nthcdr 0 '(a b c)) ;;(a b c)
(nthcdr 10 '(a b c)) ;;nil

(nth 0 '(a b c)) ;;a
(nth 3 '(a b c d)) ;;d
(nth 4 '(a b)) ;;nil

(mapcar '(lambda (x) (capitalize x)) '("lisp" "is" "cool"))
;;("Lisp" "Is" "Cool")

(setq x (list 1 2 3))
(setq y (list 1 2 3))
(eq x y) ;;nil
(equal x y) ;;t
(eq x x) ;;t
(setq c x)
(eq x c) ;;t

(assoc 'green '((red . "ff0000") (green . "00ff00") (blue . "0000ff"))) ;;(green . "00ff00")
(assoc 'titi '((red . "ff0000") (green . "00ff00") (blue . "0000ff"))) ;;nil
(assoc 'green '((red "ff0000") (green "00ff00") (blue "0000ff"))) ;;(green "00ff00")

;; Section 6. Destructive list operations.
(append '(a b) 3) ;;(a b . 3)
(setq x '(a b c))
(setq y '(d e f))
(setq z '(g h i))
(append x y z) ;;(a b c d e f g h i)

x ;;(a b c)
y ;;(d e f)
z ;;(g h i)

(nconc x y z) ;;(a b c d e f g h i)
x ;;(a b c d e f g h i)
y ;;(d e f g h i)
z ;;(g h i)

(setq e-addrs '(("robin" . "robin@wood.uk")
                ("maria" . "mf@sherwood.uk")
                ("john" . "john.doe@wood.uk")))

(defun alist-replace (alist key nvalue)
  (if (null alist)
      nil
    (if (and (listp (car alist))
             (equal (car (car alist)) key))
        (cons (cons key nvalue) (cdr alist))
      (cons (car alist) (alist-replace (cdr alist) key nvalue)))))

(setq alist '((a . b) (c . d))) ;;((a . b) (c . d))
(setq alist-2 alist)
(setq alist (alist-replace alist 'c 'q)) ;;((a . b) (c . q))
alist ;;((a . b) (c . q))
alist-2 ;;((a . b) (c . d))

(setq x (cons 'a 'b)) ;;(a . b)
(setcar x 'c) ;;c
x ;;(c . b)
(setcdr x 'd) ;;d
x ;;(c . d)

(defun alist-replace (alist key nval)
  (let ((sublist (assoc key alist)))
    (if sublist
        (setcdr sublist nval))))


(setq alist '((a . b) (c . d))) ;;((a . b) (c . d))
(setq alist-2 alist)
(alist-replace alist 'c 'q)
alist ;;((a . b) (c . q))
alist-2 ;;((a . b) (c . q))

(setq x '(a b c))
(nreverse x) ;;(c b a)
x ;;(a)

(setq x '(a b c))
(setq x (nreverse x)) ;;(c b a)
x ;;(c b a)

;; Section 7. Circular lists.
(setq x '(a b x))
(progn
  (setcdr (nthcdr 2 x) x)
  nil)
(nth 0 x) ;;a
(nth 1 x) ;;b
(nth 42 x) ;;a
(nth 43 x) ;;b
(nth 44 x) ;;x
(nth 45 x) ;;a











