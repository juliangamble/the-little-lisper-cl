;The Little LISPER - Chapter 7

;Probably need to put the definition of numbered? in here

(setf l1 '())
(setf l2 '(3 + (66 6)))
(setf aexp4 5)

;7.1
(defun mk+exp (aexp1_ aexp2_)
  (cons aexp1_
        (cons '+
              (cons aexp2_ '()))))

(mk+exp 1 2)

(defun mk*exp (aexp1_ aexp2_)
  (cons aexp1_
        (cons '*
              (cons aexp2_ '()))))

(mk*exp 1 2)

(defun mk^exp (aexp1_ aexp2_)
  (cons aexp1_
        (cons '^
              (cons aexp2_ '()))))

(mk^exp 1 2)

(setf aexp1 (mk+exp 1 (mk*exp 3 4)))
(setf aexp2 (mk+exp (mk^exp 3 4) 5))
(setf aexp3 (mk*exp 3 (mk*exp 4 (mk*exp 5 6))))

(defun operator (aexp_)
  (car (cdr aexp_)))

(operator '(1 + 2))

(defun isoperator (a)
  (cond
   ((null a) NIL)
   ((eq a '+) t)
   ((eq a '*) t)
   ((eq a '^) t)
   (t NIL)))

(isoperator '^)

(defun 1st-sub-expr (aexp_)
  (car aexp_))

(1st-sub-expr '(1 + 2))

(defun 2nd-sub-expr (aexp_)
  (car (cdr (cdr aexp_))))

(2nd-sub-expr '(1 + 2))

(defun number_ (n)
  (cond
   ((null n) t)
   (t (and
       (null (car n))
       (number_ (cdr n))))))

(cons '()(cons '() '()))
(number_ (cons '()(cons '() '())))
;(integer 1)
;(number_ (car '(1 + 2)))


(defun sub1 (n)
  (- n 1))

(defun notatom (lat)
  (not (atom lat)))

(defun number__ (n)
  (cond
   ((null n) nil)
   ((notatom n) nil)
  ((= 0 n) t)
   (t (number__ (sub1 n)))))

(number__ 10)

(number__ '(66 6))
;(sub1 (66 6))
;(number 10)

(defun aexp? (aexp_)
  (cond
   ((null aexp_) NIL)
   ((number__ aexp_) t)
   ((isoperator (operator aexp_))
    (aexp? (1st-sub-expr aexp_))
    (aexp? (2nd-sub-expr aexp_))
    )
   (t NIL)))

(aexp? aexp1)
(aexp? aexp2)
(aexp? l1)
(aexp? l2)

;7.3
(defun count-op (aexp_)
  (cond
   ((null aexp_) 0)
   ((number__ aexp_) 0)
   ((isoperator (operator aexp_))
    (+ 
    (+ 1 (count-op (1st-sub-expr aexp_)))
    (count-op (2nd-sub-expr aexp_)))
    )
   (t 0)))

(count-op '(1 + 2))
(count-op aexp1)
(count-op aexp3)
(count-op aexp4)

(defun countatomplus (a)
  (cond
   ((null a) 0)
   ((eq a '+) 1)
   (t 0)))

(countatomplus '*)
(countatomplus '+)


(defun count+ (aexp_)
  (cond
   ((null aexp_) 0)
   ((number__ aexp_) 0)
   ((isoperator (operator aexp_))
    (+ 
    (+ (countatomplus(operator aexp_)) (count+ (1st-sub-expr aexp_)))
    (count+ (2nd-sub-expr aexp_)))
    )
   (t 0)))

(count+ aexp1)

(defun countatomtimes (a)
  (cond
   ((null a) 0)
   ((eq a '*) 1)
   (t 0)))

(defun count* (aexp_)
  (cond
   ((null aexp_) 0)
   ((number__ aexp_) 0)
   ((isoperator (operator aexp_))
    (+ 
    (+ (countatomtimes(operator aexp_)) (count* (1st-sub-expr aexp_)))
    (count* (2nd-sub-expr aexp_)))
    )
   (t 0)))

(count* aexp1)

(defun countatomexp (a)
  (cond
   ((null a) 0)
   ((eq a '^) 1)
   (t 0)))

(defun count^ (aexp_)
  (cond
   ((null aexp_) 0)
   ((number__ aexp_) 0)
   ((isoperator (operator aexp_))
    (+ 
    (+ (countatomexp(operator aexp_)) (count^ (1st-sub-expr aexp_)))
    (count^ (2nd-sub-expr aexp_)))
    )
   (t 0)))

(count^ aexp1)

;7.4
(defun count-numbers (aexp_)
  (cond
   ((null aexp_) 0)
   ((number__ aexp_) 1)
   ((isoperator (operator aexp_))
    (+ 
    (count-numbers (1st-sub-expr aexp_))
    (count-numbers (2nd-sub-expr aexp_))))
   (t 0))) 

(count-numbers '(1 + 2))
(count-numbers aexp1);3;correct
(count-numbers aexp3);4;correct
(count-numbers aexp4);1;correct

;7.5
(defun number? (n)
  (cond
   ((null n) t)
   (t (and
       (null (car n))
       (number? (cdr n))))))
;?????
;a number is either zero or it is one added to a number

;(number? '2)

(defun numbered? (aexp_)
  (cond
   ((atom aexp_) (numberp aexp_))
   ;((atom aexp_) (number? aexp_))
   (t (and
       (numbered? (car aexp_))
       (numbered?
        (car (cdr (cdr aexp_))))))))
;numbered? tests whether a representation of an arithmetic expression only contains numbers besides the +, * and ^

(numbered? '(1 + a))

;value returns what we think is the natural value of a numbered arithemetic expression
;(defun value (aexp_)
;  (cond
;   ((numberp aexp_) aexp_)
;   ((eq (operator aexp) '+)
;    (+ (value (1st-sub-exp aexp_))
;       (value (2nd-sub-exp aexp_))))
;   ((eq (operator aexp) '*)
;    (* (value (1st-sub-exp aexp_))
;       (value (2nd-sub-exp aexp_))))
;   (t
;    (^ (value (1st-sub-exp aexp_))
;       (value (2nd-sub-exp aexp_))))))

;define numbered? for arbitrary length list
(defun numbered_? (aexp_)
  (cond
   ((null aexp_) t)
   ((notatom (car aexp_))
    (and 
     (numbered? (car aexp_))
     (numbered_? (cdr aexp_))))
   ((numberp (car aexp_))
    (numbered_? (cdr aexp_)))
   (t NIL)))
    
(defun numbered? (aexp_)
  (cond
   ((null aexp_) NIL)
   ((isoperator (car aexp_))
    (numbered_? (cdr aexp_)))
   (t nil)))

(numbered?'(+ 1 2 3 4 5))
(numbered?'(+ 1 2 3 (* 3 4)))

;define value? for an arbitrary length list
(defun addvec (vec)
  (cond 
   ((null vec) 0)
   ((notatom (car vec))
    (+ (value_ (car vec)) 
       (addvec (cdr vec))))
   (t (+ (car vec)(addvec (cdr vec))))))

(addvec '(1 2 3))

(defun multvec (vec)
  (cond 
   ((null vec) 1)
   ((notatom (car vec))
    (* (value_ (car vec)) 
       (multvec (cdr vec))))
   (t (* (car vec)(multvec (cdr vec))))))

(multvec '(1 2 3))

(defun ^_ (n m)
  (cond
   ((= 0 m) 1)
   (t (* n (^_ n (sub1 m))))))

(^_ 2 3)

(defun value_ (aexp_)
  (cond
   ((numberp aexp_) aexp_)
   ((notatom (car aexp_))
    (value_ (car aexp_))
    (value_ (cdr aexp_)))
   ((eq (car aexp_) '+)
    (addvec (cdr aexp_)))
   ((eq (car aexp_) '*)
    (multvec (cdr aexp_)))
   (t
    (^_ (value_ (1st-sub-expr aexp_))
       (value_ (2nd-sub-expr aexp_))))))

(value_ '(+ 3 2 (* 7 8)))
(value_ '(* 3 4 5 6))

;7.6
(setf lexp1 '(AND (OR x y) y))
(setf lexp2 '(AND (NOT y)(OR u v)))
(setf lexp3 '(OR x y))
(setf lexp4 'z)

(defun loperator? (a)
  (cond
   ((null a) NIL)
   ((eq a 'OR) t)
   ((eq a 'AND) t)
   ((eq a 'NOT) t)
   (t NIL)))

(loperator? 'AND)
(loperator? 'bob)

(defun lexp? (lexp_)
  (cond
   ((null lexp_) NIL) 
   ((atom lexp_) t)
   ((loperator? (car lexp_))
    (cond 
     ((eq (car lexp_) 'NOT)
      (lexp? (1st-sub-expr lexp_)))
     (t 
       (lexp? (1st-sub-expr lexp_))
       (lexp? (2nd-sub-expr lexp_)))))
   (t NIL)))

(lexp? lexp1);T;correct
(lexp? lexp2);T;correct
(lexp? lexp3);T;correct
(lexp? aexp1);NIL;correct
(lexp? l2);NIL;correct

;7.7
(defun add1 (n)
  (cond
   ((null n) '())
   ((+ n 1))))

(add1 1)

(defun occurNa (a1 lat)
  (cond
   ((null lat) 0)
   ((null a1) 0)
   (t (cond
       ((eq (car lat) a1)
        (add1 (occurNa a1 (cdr lat))))
       (t (occurNa a1 (cdr lat)))))))

(occurNa 'c (list 'a 'b 'c))
(occurNa 'bananas lat2)

(defun occurN (alat lat)
  (cond 
   ((null alat) 0)
   ((null lat) 0)
   (t (+ (occurNa (car alat) lat)
      (occurN (cdr alat)  lat)))))

(occurN (list 'bananas) (list 'bananas 'peaches 'bananas))

(defun occur* (a lat)
  (cond
   ;((null lat) NIL)
   ;((notatom (car lat))
   ; (or
   ;  (occur* a (car lat))
   ;  (occur* a (cdr lat))))
   ((eq a (car lat) ) t)
   (t NIL)))
   ; (occur* a (cdr lat)))
;))

(defun occur* (a lat)
  (cond
   ((null lat) NIL)
   ((notatom (car lat))
    (or
     (occur* a (car lat))
    (occur* a (cdr lat))))
   ((eq a (car lat) ) t)
   (t (occur* a (cdr lat)))))
   
(occur* 'bananas '(bananas peaches))
(occur* 'peaches '(bananas peaches))
(null '(bananas peaches))
(notatom (car '(bananas peaches)))
(eq 'bananas (car '(bananas peaches)))
(eq 'peaches (car '(bananas peaches)))
(eq 'peaches (car '(bananas peaches)))

(cond
 ((= 0 1) t)
 ((eq 'peaches (car '(bananas peaches))))
 (t NIL))

(occur* 'y '(x y))


(defun 1st-sub-expr (aexp_)
  (car (cdr aexp_)))

(1st-sub-expr '(+ 1 2))

(defun 2nd-sub-expr (aexp_)
  (car (cdr (cdr aexp_))))

(2nd-sub-expr '(+ 1 2))

(defun operator (aexp_)
  (car aexp_))

(operator '(NOT x))

(defun covered? (lexp_ lat)
  (cond
   ((null lexp_) NIL) 
   ;((not (lexp? lexp_)) NIL)
   ((atom lexp_) 
    (occur* lexp_ lat))
   (t (cond
       ((eq (operator lexp_) 'NOT)
        (covered? (1st-sub-expr lexp_) lat))
       (t 
        (and 
         (covered? (1st-sub-expr lexp_) lat)
         (covered? (2nd-sub-expr lexp_) lat)))))))

(covered? lexp1 '(x y z u));T;correct
(covered? lexp2 '(x y z u));F;correct
(covered? lexp4 '(x y z u));T;correct
(covered? '(NOT x) '(x))
(covered? '(NOT x) '(x y))
(covered? '(AND x y) '(x y))
(occur* 'y '(x y))

;(setf lexp1 '(AND (OR x y) y))
(covered? '(OR x y) '(x y z u))
(occur* 'x '(x y z u))
(covered? 'y '(x y z u))      

;7.8
(defun lookup (a lat)
  (cond
   ((null a) NIL)
   ((null lat) NIL)
   ((eq a (car (car lat)))
    (cdr (car lat)))
   (t 
    (lookup a (cdr lat)))))

(lookup 'y '((x 1)(y 0)));0;correct
(lookup 'x '((x 1)(y 0)))
(lookup 'u '((u 1)(v 1)));1;correct
(lookup 'y '());NIL;correct

;7.9
(defun Mlexp (lexp_ alist_)
  (cond
   ((null lexp_) NIL)
   ((null alist_) NIL)
   ((atom lexp_)
    (cond
     ;((eq lexp_ 't) t)
     ((= 1 (car(lookup lexp_ alist_))) t)
     (t NIL)))
   ((eq (operator lexp_) 'AND)
    (cond
    ((and
     (Mlexp (1st-sub-expr lexp_) alist_)
     (Mlexp (2nd-sub-expr lexp_) alist_))
     't)
     (t NIL)))
   ((eq (operator lexp_) 'OR)
    (cond
    ((or
     (Mlexp (1st-sub-expr lexp_) alist_)
     (Mlexp (2nd-sub-expr lexp_) alist_))
     't)
     (t NIL)))
    ((eq (operator lexp_) 'NOT)
     (cond
    ((not
     (Mlexp (1st-sub-expr lexp_) alist_))
      't)
     (t NIL)))
   (t NIL)))

(setf lexp1 '(AND (OR x y) y))
(Mlexp lexp1 '((x 1)(y 0)(z 0)));F;correct
(Mlexp lexp2 '((y 0)(u 0)(v 1)));T;correct
(Mlexp lexp4 '((x 1)(y 0)(z 0)));F;correct

;7.10
(OR t f t f)
(AND t NIL t NIL)

(defun Mlexp-OR (lat alist)
  (cond
   ((null alist) NIL)
   ((null lat) NIL)
   (t
    (OR
     (Mlexp (car lat) alist)
     (Mlexp-OR (cdr lat) alist)))))

(Mlexp-OR '(x y z) '((x 1)(y 1)(z 0)))
(Mlexp-OR '(x y z) '((x 1)))
(lookup 'x '((x 1)(y 1)(z 0)))

(defun Mlexp-AND (lat alist)
  (cond
   ((null alist) NIL)
   ((null lat) t)
   (t
    (AND
     (Mlexp (car lat) alist)
     (Mlexp-AND (cdr lat) alist)))))

(Mlexp-AND '(x y z) '((x 1)(y 1)(z 0)))
(Mlexp-AND '(x) '((x 0)))
(Mlexp-AND '(x) '((y 1)(x 0)))
(Mlexp-AND '(x y) '((x 1)(y 1)(z 0)))

(defun Mlexp (lexp_ alist_)
  (cond
   ((null lexp_) NIL)
   ((null alist_) NIL)
   ((atom lexp_)
    (cond
     ;((eq lexp_ 't) t)
     ((= 1 (car(lookup lexp_ alist_))) t)
     (t NIL)))
   ((eq (operator lexp_) 'AND)
    (Mlexp-AND (cdr lexp_) alist_))
   ((eq (operator lexp_) 'OR)
     (Mlexp-OR (cdr lexp_) alist_))
   ((eq (operator lexp_) 'NOT)
     (cond
    ((not
     (Mlexp (1st-sub-expr lexp_) alist_))
      't)
     (t NIL)))
   (t NIL)))

(Mlexp lexp1 '((x 1)(y 0)(z 0)));F;correct
;(setf lexp2 '(AND (NOT y)(OR u v)))
(Mlexp lexp2 '((y 0)(u 0)(v 1)));T;correct
(Mlexp '(OR u v) '((y 0)(u 0)(v 1)));
(Mlexp '(OR u (OR u v)) '((y 0)(u 0)(v 1)));
(Mlexp '(NOT y) '((y 0)(u 0)(v 1)));
(Mlexp '(AND v v) '((y 0)(u 0)(v 1)));
(Mlexp '(AND (AND v v) v) '((y 0)(u 0)(v 1)));
(Mlexp lexp4 '((x 1)(y 0)(z 0)));F;correct

(Mlexp '(AND x y z) '((x 1)(y 0)(z 0)));F;correct
(Mlexp '(NOT(AND x y z)) '((x 1)(y 0)(z 0)));T;correct
(Mlexp '(OR z z (NOT(AND x y z))) '((x 1)(y 0)(z 0)));T;correct

    
   
   











    

