;The Little Lisper Chapter 9.

(lambda (a) 
     (lambda (x)
          (eq x a)))

(lambda (x y) 
  (+ x y))

(defun member? 
    (lambda (a lat)
  (cond
   ((null lat) NIL)
   (t (or
       (eq (car lat) a)
       (member? a (cdr lat)))))))

;thoughts
;explain different eq functions
;explain where common lisp functions have been renamed
;explain use of (lambda for dsls...
;explain expansions for more universal cases...
;explain that not all functions are needed - just the essentials to define things in terms of themselves
;explain that lambdas aren't required - but are inserted to demonstrate the y-combinator later on

;9.1
(defun firsts (l)
  (cond
   ((null l) NIL)
   (t (cons (car (car l)) 
                 (firsts (cdr l))))))

(firsts '((paella spanish)(wine red)(and beans)))

(defun seconds (l)
  (cond
   ((null l) NIL)
   (t (cons (car (cdr (car l))) 
            (seconds (cdr l))))))

(seconds '((paella spanish)(wine red)(and beans)))

(defun firsts-lambda (l)
  (lambda (l)
    (car (car l))))

(firsts-lambda '((paella spanish)(wine red)(and beans)))

(defun rember-f
    (lambda (test?)
      (lambda (a l)
        (cond
         ((null l) (quote()))
         ((test? (car l) a) (cdr l))
         ;(funcall (test? (car l) a) (cdr l))
         (t (cons (car l)
            ((rember-f test?)
             a (cdr l))))))))

(defun rember-f (test? a l)
    (cond
     ((null l) (quote ()))
     ((funcall test? (car l) a) (cdr l))
     (t (cons (car l) (rember-f test? a (cdr l))))))

(rember-f (function =) 5 '(6 2 5 3))

(defun rember-f (test?)
    (function
     (lambda (a l)
       (cond
        ((null l) (quote()))
        ((funcall test? (car l) a) (cdr l))
        (t (cons (car l) (funcall (rember-f test?) a (cdr l))))))))

(funcall (rember-f #'eq) 'tuna '(tuna salad is good))
(funcall (rember-f #'eq) 'tuna '(shrimp salad and tuna salad))



(defun rember-eq? (rember-f eq))

(rember-eq? 'tuna '(tuna salad is good))
(funcall (rember-f 'eq) 'tuna '(tuna salad is good))


(eq 'tuna 'tuna)
    
;(defun map_ 
;    (lambda  (f)
;      (lambda (l)
;        (cond
;         ((null l) NIL)
;         (t (cons((map_ f) f l))            
;            (map_ f (cdr l)))))))      

;(map_  
;        (lambda (l)
;          (car (car l)))
;      '((paella spanish)(wine red)(and beans)))

;(map_ (lambda (l) 
;       (lambda (l)
;          (car (car l))))
;      '((paella spanish)(wine red)(and beans)))

(defun firsts (l)
  (cond
   ((null l) NIL)
   (t (cons (car (car l)) 
                 (firsts (cdr l))))))

(firsts '((paella spanish)(wine red)(and beans)))

;------------------------------------
(defun map_ (l func)
  (cond
   ((null l) NIL)
   (t (cons (funcall func (car l))
            (map_ (cdr l) func)))))

;firsts
(map_ '((paella spanish)(wine red)(and beans)) #'car)

;------------------------------------
; YAY!
;------------------------------------
(setf spanish-food '((paella spanish)(wine red)(and beans)))
;firsts
(map_ spanish-food #'car)
;seconds
(defun seconds-closure (l)
  (car (cdr l)))

(map_ spanish-food #'seconds-closure)
    

;9.2
(defun pair? (lat)
  (cond
   ((null lat) NIL)
   ((atom lat) NIL)
   ((and (and (not (eq (first_ lat) NIL))
              (not (eq (second_ lat) NIL))))
    (eq (third lat) NIL))
   (t NIL)))

(defun first_ (l)
  (cond
   ((null l) '())
   (t (car l))))

(defun second_ (l)
  (cond
   ((null l) '())
   (t (car (cdr l)))))

(defun eq-pair-first (pair-a-first pair-b)
  (cond
   ((null pair-a-first) NIL)
   ((null pair-b) NIL)
   ((not(atom pair-a-first)) NIL)
   ((atom pair-b) NIL)
   ((not (pair? pair-b)) NIL)
   ((eq pair-a-first 
             (first_ pair-b)))
   (t NIL)))

(eq-pair-first 'a '(a b))
(eq-pair-first 'a '(b a))

(defun rel? (rel)
  (cond
   ((null rel) t)
   ((atom rel) NIL)
   ((pair? (car rel))
    (rel? (cdr rel)))
   (t NIL)))

;(defun exists? (first rel)   
;  (cond
;   ((null first) t)
;   ((null rel) NIL)
;   ((not(rel? rel)) NIL)
;   ((eq-pair-first first (car rel)) t)   
;   (t (member-pair-firsts? first (cdr rel)))))

(defun exists? (first rel)   
  (cond
   ((null first) t)
   ((null rel) NIL)
   ((not(rel? rel)) NIL)
   ((eq-pair-first first (car rel)) t)   
   (t (exists? first (cdr rel)))))

(exists? 'a '((a b)(a c)(a a)))
(exists? 'd '((a b)(a c)(a a)))
(eq-pair-first 'a '(a b))
(rel? '((a b)(a c)(a a)))

   
(defun find-match (a l)
  (cond
   ((null a) '())
   ((null l) NIL)
   ((not(rel? l)) NIL)
   ((eq-pair-first a (car l) ) (car l))   
   (t (find-match a (cdr l)))))

(find-match 'a '((a b)(a c)(a a)))

(defun assq-sf (a l sk fk)
  (cond
   ((exists? a l)
    (funcall sk (find-match a l)))
   (t (funcall fk a))))

(defun build (a b)
  (cons a (cons b '())))

(setf a 'apple)
(setf b1 '())
(setf b2 '((apple 1)(plum 2)))
(setf b3 '((peach 3)))
(setf sk (lambda (p)(build (first p) (add1 (second p)))))
(setf fk (lambda  (name) (cons name (quote (not-in-list)))))

(assq-sf a b1 sk fk)
(assq-sf a b2 sk fk)
(assq-sf a b3 sk fk)

;--------------------------
; YAY
;---------------------------

;9.3

;Y-combinator for functions of two arguments
(defun Y2 ()
    (lambda (M)
      ((lambda (future)
         (funcall (M) (lambda (arg1 arg2)
              ((funcall (future) future) arg1 arg2))))
       (lambda (future)
         (funcall (M) (lambda (arg1 arg2)
                        ((funcall (future) future) arg1 arg2)))))))
;rewritten for lisp-2 namespace - and removed first lambda
(defun Y2 (M)
      ((lambda (future)
         (funcall M (function (lambda (arg1 arg2)
                                (funcall (funcall future future) arg1 arg2)))))       
                                
       (function (lambda (future)                                      
                   (funcall M (lambda (arg1 arg2)                                
                                (funcall (funcall future future) arg1 arg2)))))))

;Ch4: =
(defun =_ ()
    (lambda (n m)
      (cond
       ((> n m) nil)
       ((< n m) nil)
       (t t))))

(funcall (=_)  1 2)
(funcall (=_)  1 1)

(defun =_M ()
  (lambda (recfun)
    (lambda (n m)
      (cond
       ((> n m) nil)
       ((< n m) nil)
       (t t)))))

(funcall (funcall (=_M) '()) 1 2)

(funcall (Y2) '=_M)

(defun =_Y () (funcall (Y2) '=_M))

(funcall (=_Y) 1 1)
(funcall (=_Y 1 2)) 

(setf (symbol-function '_=)
  (Y2 (function (lambda (recurring-function)
                 (function (lambda (m n)
                                   (cond
                                    ((> n m) nil)                                    
                                    ((< n m) nil)                                    
                                    (t t))))))))
;perhaps this should be the recursive version of eq - that subs down to zero...
(_= 1 2)
(_= 2 2)

;Ch4 - pick
;lamda syntax
(defun pick ()
  (lambda (n lat)
    (cond
     ((null lat) nil)
     ((zero (sub1 n)) (car lat))
     (t (funcall (pick) (sub1 n) (cdr lat))))))

(funcall (pick) 3 '(a b c d e f g h i j))
;regular syntax
(defun pick (n lat)
    (cond
     ((null lat) nil)
     ((zero (sub1 n)) (car lat))
     (t (pick (sub1 n) (cdr lat)))))

(pick 3 '(a b c d e f g h i j))

(setf (symbol-function 'pick)
  (Y2 (function (lambda (recurring-function)
                 (function (lambda (n lat)
                             (cond
                              ((null lat) nil)
                              ((zero (sub1 n)) (car lat))
                              (t (funcall recurring-function (sub1 n) (cdr lat))))))))))
 
(pick 3 '(a b c d e f g h i j))                           

;Ch4 - rempick
(defun rempick ()
  (lambda (n lat)
    (cond
     ((null lat) '())
     ((zero (sub1 n)) (cdr lat))
     (t (cons (car lat)
              (funcall (rempick) (sub1 n)(cdr lat)))))))

(funcall (rempick) 3 '(a b c d e f g h i j))

(setf (symbol-function 'rempick)
  (Y2 (function (lambda (recurring-function)
                 (function (lambda (n lat)
                             (cond
                              ((null lat) '())
                              ((zero (sub1 n)) (cdr lat))
                              (t (cons (car lat)
                                       (funcall recurring-function (sub1 n)(cdr lat)))))))))))

(rempick 3 '(a b c d e f g h i j))
;--------------------
;YAY!
;--------------------

;Thoughts
: There has to be a better way than symbol-function

;9.4

;assuming reference to Y  - refers to Y2 and not to a hypothetical Y function with a list of arguments

;assuming reference to member refers to chapter 2 (for member function) and not 3 (typo)
(defun member? (a lat)
  (cond 
   ((null lat) nil)
   (t (or
       (eq (car lat) a)
       (member? a (cdr lat))))))

(member? 'c '(a b c d e f g))
(member? 'z '(a b c d e f g))

;(defun member-Y (a l)
;  ((Y2 (lambda (recursive-function)
;        (lambda (l)
(setf (symbol-function 'member-Y)
  (Y2 (function (lambda (recurring-function)
                  (function (lambda (a lat)
                              (cond
                               ((null lat) nil)
                               (t (or
                                   (eq (car lat) a)
                                   (funcall recurring-function a (cdr lat)))))))))))
      ;l)))

(member-Y 'a '(a b c d e f g))
(member-Y 'b '(a b c d e f g))
(member-Y 'c '(a b c d e f g))
(member-Y 'z '(a b c d e f g))
;but rember, does come from chapter 3

(defun rember (a lat)
  (cond 
   ((null lat) '())
   (t (cond
       ((eq (car lat) a) (cdr lat))
       (t (cons (car lat)
                (rember
                 a (cdr lat))))))))

(rember 'and '(bacon lettuce and tomato))

(setf (symbol-function 'rember-Y)
  (Y2 (function (lambda (recurring-function)
                  (function (lambda (a lat)
                              (cond 
                               ((null lat) '())
                               (t (cond
                                   ((eq (car lat) a) (cdr lat))
                                   (t (cons (car lat)                                            
                                   (funcall recurring-function a (cdr lat)))))))))))))

(rember-Y 'and '(bacon lettuce and tomato))

;insertR
(defun insertR (new old lat)
  (cond
   ((null lat) '())
   (t (cond
       ((eq (car lat) old)
        (cons old
              (cons new (cdr lat))))
       (t (cons (car lat)
                (insertR
                 new old (cdr lat))))))))

(insertR 'bob 'jane '(bob jane tmart))

(defun YN (F)
   ((lambda (future)
      (funcall F (function(lambda (&rest args)
                     (apply (funcall future future) args)))))
    #'(lambda (future)
        (funcall F (function(lambda (&rest args)
                       (apply (funcall future future) args))))) ) ) 

(setf (symbol-function 'insertR-Y)
  (YN (function (lambda (recurring-function)
                  (function (lambda (new old lat)
                              (cond
                               ((null lat) '())
                               (t (cond
                                   ((eq (car lat) old)
                                    (cons old
                                          (cons new (cdr lat))))
                                   (t (cons (car lat)
                                            (funcall recurring-function
                                             new old (cdr lat)))))))))))))

(insertR-Y 'bob 'jane '(bob jane tmart))


;subst-2
(defun subst2 (new o1 o2 lat)
  (cond
   ((null lat) '())
   (t (cond
       ((eq (car lat) o1)
        (cons new (cdr lat)))
       ((eq (car lat) o2)
        (cons new (cdr lat)))
       (t (cons (car lat)
                (subst2 new
                        o1 o2 (cdr lat))))))))

(subst2 'vanilla 'chocolate 'banana '(banana icecream with chocolate topping))
 
(setf (symbol-function 'subst2-Y)
  (YN (function (lambda (recurring-function)
                  (function (lambda (new o1 o2 lat)
                              (cond
                               ((null lat) '())
                               (t (cond
                                   ((eq (car lat) o1)
                                    (cons new (cdr lat)))
                                   ((eq (car lat) o2)
                                    (cons new (cdr lat)))
                                   (t (cons (car lat)
                                            (funcall recurring-function new
                                                     o1 o2 (cdr lat)))))))))))))

(subst2-Y 'vanilla 'chocolate 'banana '(banana icecream with chocolate topping))
(subst2-Y 'vanilla 'chocolate 'topping '(banana icecream with chocolate topping))

;--------------------------------------
;    YAY!
;--------------------------------------

;9.5
;Note multi-subst is from chapter 5 exercises - accumulators in 6
;original
(setf x 'comma)
(setf y 'dot)
(setf a 'kiwis)
(setf b 'plums)
(setf lat1 '(bananas kiwis))

(defun multisubst2 (new o1 o2 lat)
  (cond
  ((null lat) '())
  (t (cond 
      ((eq (car lat) o1)
       (multisubst2 new o1 o2 (cons new (cdr lat))))
      ((eq (car lat) o2)
       (multisubst2 new o1 o2 (cons new (cdr lat))))
      (t (cons (car lat)
               (multisubst2 new 
                            o1 o2 (cdr lat))))))))

(multisubst2 x a b lat1);(bananas comma);correct
(multisubst2 y a b lat3);(dot pears dot bananas gerries);correct
(multisubst2 a x y lat1);(bananas kiwis);correct

(defun multisubst (new old lat)
  (cond
   ((null lat) '())
   (t (cond
       ((eq (car lat) old)
        (cons new 
              (multisubst 
               new old (cdr lat))))
       (t (cons (car lat)
                (multisubst
                 new old (cdr lat))))))))

(multisubst 'bob 'jane '(bob jane bob jane))

(defun multisubst-k (new old lat k)
  (cond
   ((null lat) (funcall k '()))
   ((eq (car lat) old)
    (multisubst-k new old (cdr lat)
                  (lambda (d)
                    (funcall k (cons new d)))))
   (t (multisubst-k new old (cdr lat)
                    (lambda (d)
                      (funcall k (cons (car lat) d)))))))

(multisubst-k 'bob 'jane '(bob jane bob jane) (function (lambda (x) x)))
(multisubst-k 'y 'x '(u v x x y z z) (function (lambda (x) x)))

;Comparison of steps

;Things you need to do when you return from a recursive function ;corresponding continuation function
; Instead of just returning a (quote()) function - you need to send the quote 
; function to the continuation - and let it return it
; otherwise you call the continuation to escape the recursion ?
; so instead of consing the result of the recursion on the parent function
; you're consing the results of the continuation...

;----
; YAY
;----
;9.6
;assume addvec is the text and multivec is the exercise from chapter 5
(defun addvec (vec)
  (cond
   ((null vec) 0)
   (t (+ (car vec) (addvec (cdr vec))))))
(addvec '(1 2 3))

(defun multvec (lat)
  (cond
   ((null lat) 1)
   (t (* (car lat)
         (multvec (cdr lat))))))

(multvec '(2 3 4 5 6))

(defun accum ( func terminator lat)
  (cond
   ((null lat) terminator)
   (t (funcall func (car lat)
               (accum func terminator (cdr lat))))))

(accum '+ 0 '(1 2 3 4 5 6) )
(accum '* 1 '(1 2 3 4 5 6) )

;-----------
; YAY!
;-----------

; 9.7
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

(operator '(1 + 2))
(isoperator (operator '(1 + 2)))

(count-op '(1 + 2))


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

(count+ '(1 + 1))

(defun countatomtimes (a)
  (cond
   ((null a) 0)
   ((eq a '*) 1)
   (t 0)))

(countatomtimes '*)

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

(count* '(1 * 1))

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

(count^ '(1 ^ 1))

(defun count-op-f (aexp_ op-function)
  (cond
   ((null aexp_) 0)
   ((number__ aexp_) 0)
   ((isoperator (operator aexp_))
    (+ 
     (+ (funcall op-function (operator aexp_))
     (count-op-f (1st-sub-expr aexp_) op-function))
    (count-op-f (2nd-sub-expr aexp_) op-function))
    )
   (t 0)))

(count-op-f '(1 + 1) (function (lambda (a) (cond ((null a) 0)((eq a '+) 1)(t 0)))))
(count-op-f '(1 + 1)  (lambda (a) (cond ((null a) 0)((eq a '+) 1)(t 0))))
(count-op-f '(1 * 1)  (lambda (a) (cond ((null a) 0)((eq a '*) 1)(t 0))))
(count-op-f '(1 ^ 1)  (lambda (a) (cond ((null a) 0)((eq a '^) 1)(t 0))))

(defun count-op-f2 (aexp_ op-function)
  (cond
   ((null aexp_) 0)
   ((number__ aexp_) 0)
   ((eq (operator aexp_) op-function)
    (+ 
     (+ 1
       (count-op-f (1st-sub-expr aexp_) op-function))
    (count-op-f (2nd-sub-expr aexp_) op-function))
    )
   (t 0)))

(count-op-f2 '(1 + 1)  '+)
(count-op-f2 '(1 - 1)  '+)


;-----
; YAY
;-----

;9.8
;Thoughts - need to rewrite other definitions of member? and subset? based on this...
(defun mem (e x)
  (if (endp x)
      nil
    (if (equal e (car x))
        t
      (mem e (cdr x)))))

(defun subset (x y)
  (if (endp x)
      t
    (and (mem (car x) y)
         (subset (cdr x) y))))

;Need to rewrite

; Does replacing and in subset with or get you intersect?

(defun member? (a lat)
  (cond
   ((null lat) NIL)
   ((eq a (car lat)) t)
   (t
    (member? a (cdr lat)))))

(member? 'x '(a b c d e f))
(member? 'f '(a b c d e f))

(defun subset? (lat lat2)
  (cond
   ((null lat) t)
   (t
    (and (member? (car lat) lat2)
         (subset? (cdr lat) lat2)))))

(subset? '(a b) '(a c d))
(subset? '(a c) '(a c d))

(defun intersect? (lat lat2)
  (cond
   ((null lat) t)
   (t
    (or (member? (car lat) lat2)
         (intersect? (cdr lat) lat2)))))

(intersect? '(a b) '(a c d))
(intersect? '(a c) '(a c d))

(defun or-func (or1 or2)
      (or (funcall or1)(funcall or2)))

(defun and-func (or1 or2)
      (and (funcall or1)(funcall or2)))

(setf (symbol-function 'set-f?)
  (lambda (current-function)
  (function 
   (lambda (cond1 cond2)
     (funcall current-function cond1 cond2)))))

(funcall  (lambda () (= 1 1)))
(funcall (set-f? 'or-func) (lambda() (= 1 1)) (lambda() (= 1 2)))
(funcall (set-f? 'and-func) (lambda() (= 1 1)) (lambda() (= 1 2)))

;____________________________
; YAY!
;____________________________

;9.9

(defun first$ first)

(defun second$
    (lambda (str)
      ((second str))))

(build 1 (lambda () 2))

(first (build 1 (lambda () 2)))
(second (build 1 (lambda () 2)))
(funcall (second (build 1 (lambda () 2))))
;Typo - Assume that in scheme he meant ((second$ s)) and not (second$ s)

;(defun add1 (n) (+ 1 n))
;(add1 1)

;(defun str-maker (next n)
;  (build n (lambda () (str-maker next  (funcall next n)))))

(defun add1 ()
    (lambda (n)
      (+ 1 n)))
(funcall (add1) 1)

(defun str-maker (next n)
  (build n (lambda () (str-maker next (funcall (funcall next) n)))))

;(setf (symbol-function 'str-maker)
;  (lambda (next n)
;    (build n (lambda () (str-maker next (funcall (next) n))))))

(str-maker 'add1 0)
(defun int_ () (str-maker 'add1 0))

(int_)
;(funcall (int_ ))
(funcall 'int_)
(first (int_))
(second (int_))
;(first (second (int_)))
;(funcall (second (int_)))
;(first (funcall (second (int_))))
;(car (funcall (second (int_))))
;(cdr (funcall (second (int_))))
;(second (funcall (second (int_))))
;(funcall (second (funcall (second (int_)))))
;(first (funcall (second (funcall (second (int_))))));2
(cdr (int_))
(first (cdr (int_)))
;(funcall (cdr (int_)))
;(funcall (first (cdr (int_))))

;Notes - equivalent
(funcall '+ 1 1 )
(+ 1 1)

;???
(defun even () (str-maker (function (lambda (n) (+ 2 n)) )0))
;???

(defun add2 ()
  (lambda (n)
    (+ 2 n)))

(funcall (add2) 1)

(defun even () (str-maker 'add2 0))

(even)
;(funcall (even))
(first (even))
(second (even))
(funcall (second (even ) ) )
(first (funcall (second (funcall (second (even))))))

(mapc #'print '(a b c))
(mapc (function print) '(a b c))


(defun frontier (str n)
    (cond
     ((zero n) '())
     (t (cons (first  (funcall str)) (frontier (second (funcall str)) (sub1 n))))))

;note first and second symbols not used....

(int_)
#'int_
(frontier #'int_ 10)
(frontier (function int_) 10)
(frontier #'int_ 100)
(frontier #'even 23)

(defun odd () (str-maker 'add2 1))

(frontier #'odd 10)
(frontier (function odd) 10)
    
;----------------------
;    9.10
;----------------------
(str-maker 'add1 0)
(defun int_ () (str-maker 'add1 0))

(defun add1 ()
    (lambda (n)
      (+ 1 n)))
(funcall (add1) 1)

(defun str-maker (next n)
  (build n (lambda () (str-maker next (funcall (funcall next) n)))))


(defun Q (str n)
  (cond
   ((zero (remainder (first (funcall str)) n))
    (Q (second (funcall str)) n))
   (t (build (first (funcall str))
             (lambda ()
               (Q (second (funcall str)) n))))))

;(defun P (str)
;  (build (first (funcall str)) (lambda () (P (Q (funcall str) (first (funcall str)))))))

;(defun P ()
;  (lambda (str)
;  (build (first (funcall str)) (lambda () (P (Q (funcall str) (first (funcall str))))))))
;(defun P ()
;  (lambda (str)
;  (build (first (funcall str)) (lambda () (P (Q  str (first (funcall str))))))))
(defun P (str)
  (build (first (funcall str)) (lambda () (P (Q (funcall str) (first (funcall str)))))))

(frontier ('#P (second (funcall (second (int_)))))10 ) 
;(frontier (P (second (second (int_)))) 10)
;(P (second (second (int_))))
(int_)
(second (int_))
(funcall #'second (int_))
(funcall (funcall #'second (int_)))
(funcall (second (int_)))
;(funcall #'second #'int_)
;(second (second #'int_))
;((second (int_))0)
(funcall (second (int_)) )
(second (funcall (second (int_))))
(funcall (P) (second (funcall (second (int_)))))
;(frontier (funcall (P) (second (funcall (second (int_))))) 10)
;(frontier #'(funcall (P) (second (funcall (second (int_))))) 10)
;(frontier #'(function funcall (P) (second (funcall (second (int_))))) 10)
;(frontier #'(lambda()  funcall (P) (second (funcall (second (int_))))) 10)
;(frontier #'(lambda()  (P) (second (funcall (second (int_))))) 10)
;(frontier #'(lambda()  (P) (second (funcall (second (int_))))) 10)
;-------------------------
(frontier #'(lambda()  (P) (funcall (second (funcall (second (int_)))))) 10)
;lambda defintion of P eg defun P () (lamdba...
;--------------------------
;YAY?Incorrect - P function not called?
;--------------------------
(frontier #'(lambda()  (P) (funcall (second (funcall (second (int_)))))) 2)
(frontier #'(lambda()  ((P) (funcall (second (funcall (second (int_))))))) 2)

;simple arg version of P
(frontier #'(lambda()  (P #'(lambda () (funcall (second (funcall (second (int_)))))))) 10)
(frontier #'(P (lambda () (funcall (second (funcall (second (int_))))))) 10)
(frontier (P (lambda () (funcall (second (funcall (second (int_))))))) 10)
(P (lambda () (funcall (second (funcall (second (int_)))))))
(lambda () (P (lambda () (funcall (second (funcall (second (int_))))))))
(frontier #'(lambda () (P (lambda () (funcall (second (funcall (second (int_)))))))) 10)
(frontier #'int_ 10)



(funcall (second (funcall (second (int_)))))

(funcall (P) (second (funcall (second (int_)))))
(funcall (P)  (int_))
(frontier (funcall (P) (second (funcall (second (int_)))))10 ) 
;(function (P (second (funcall (second (int_))))))
;((P) (second (funcall (second (int_)))))
;((P) (second (funcall (second (int_)))))
;(P (second (funcall (second (int_)))))
(frontier #'odd 10)
(frontier (function odd) 10)
(odd)
(Q (lambda () (6)) 2)
(remainder 5 2)
(remainder (first (int_)) 10)
(defun R (str n)
  (remainder (first (funcall str)) 10))
(R #'int_ 10)

(defun S (str n)  
  ((zero (remainder (first (funcall str)) 10))
   (S (second (funcall str)) n)))
(S #'int_ 10)


(defun U (str n)  
  (cond
   ((zero (remainder (first (funcall str)) 10))    
    (U (second (funcall str)) n))
  (t (build (first (funcall str))
                   (lambda () 
                     (U (second (funcall str) n)))))))

(U #'int_ 10)
(Q #'int_ 10)
(second (U #'int_ 10))
(second (int_))
(funcall (second (int_)))
;(funcall (second (U #'int_ 10)) )
;((second (U #'int_ 10)))
(cdr (U #'int_ 10))
(U (cdr (U #'int_ 10)) 10)

(defun V (str)
  (build (first (funcall str)) (lambda () (V (U str (first (funcall str)))))))

(V #'int_)



