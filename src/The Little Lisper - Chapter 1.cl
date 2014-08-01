;Solutions to The Little Lisper - Part 1

;Notes - "'" is equivalent to quote() - and returns a list
;Outstanding - find the equivalent for creating an atom
; note (atom 'atom)

;1.1
'a
'b
'c
'd
'e
'f
'g
'h
'i
'j

;1.2
'(a a)
'(b a)
'(c a)
'(d a)
'(e a)
'(f a)
'(g a)
'(h a)
'(i a)
'(j a)
'(a b)
'(b b)
'(c b)
'(d b)
'(e b)
'(f b)
'(g b)
'(h b)
'(i b)
'(j b)

;1.3 
(defvar a 'all)
(defvar b 'these)
(defvar c 'problems)
(defvar d '())
a
b
c
d
(cons 'all (cons 'these (cons 'problems ())))

(cons a (cons b (cons c d)))
(cons a (cons (cons b (cons c d))d))
(cons a (cons (cons b d)(cons c d)))
(cons (cons a (cons b d )) (cons c d))
(cons (cons a (cons b (cons c d))) d)

;1.4
:up
(setf a 'french) ;since a is already set - we change it
(defvar l '(fries))

(car (cons a l))

(setf a 'oranges) ;since a is already set - we change it
(setf l '(apples and peaches))

(cdr (cons a l))

;1.5
(defvar x 'lisp)
(defvar y 'lisp)
(eq x y)
;(defvar x (car('lisp 'lisp)))
(defvar y 'lisp)
(eq x y)

;1.6
;No - see the following - 
(setf a 'bob)
(cons a nil)
(cons a ())
(cons a '())
(null (cons a nil))

;1.7
(setf s 'x)
(setf l 'y)
(cons s l)

(setf s '())
(setf l '())
(cons s l)

(setf s '())
(car s)

(setf l '('()))
(cdr l)

;1.8
(setf l '((meatballs) and spaghetti))
(car l)
(atom (car l))
;False (NIL)

(setf l '((meatballs)))
(cdr l)
;(null cdr( l)))
;False? (Error)

(setf l '(two meatballs))
(eq (car l)(car (cdr l)))
;False (NIL)

(setf l '(ball))
(setf a 'meat)
(atom (cons a l))
;False (NIL)


;1.9
(setf l '((kiwis mangoes lemons) and (more)))
(car (cdr (cdr (car l))))
;lemons

(setf l '(() (eggs and (bacon)) (for) (breakfast)))
(car (cdr (car (cdr l))))
;and

(setf l '(() () () (and (coffee)) please))
(car (cdr (cdr (cdr l))))
;(and (coffee))

;1.10
(setf l '(apples in (Harry has a backyard)))
(car(car(cdr (cdr l))))

(setf l '(apples and Harry))
(car(cdr (cdr l)))

(setf l '(((apples) and ((Harry))) in his backyard))
(car(car(car(cdr (cdr (car l))))))
