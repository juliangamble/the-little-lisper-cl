;The Little Lisper Chapter 10

(defun build (a b)
  (cons a (cons b '())))

(setf e4
     '(3 (quote a)(quote b)))


;Thoughts - add note about setf and setq in common lisp
;http://stackoverflow.com/questions/869529/difference-between-set-setq-and-setf-in-common-lisp
;

(defun value_ (e)
  (meaning e (quote ())))

(defun meaning (e table)
;  (print '--meaning--)
  ;(print e)
  ;(print table)
  (funcall (expression-to-action e) e table))

(print 'a )
(funcall #'eq 1 2)
;thoughts - note on debugging technique - add a (print ...) as a step in sequence


(defun expression-to-action (e)
  (cond
   ((atom e) (atom-to-action e))
   (t (list-to-action e))))

(defun atom-to-action (e)
  (cond
   ((numberp e) '*self-evaluating)
   (t '*identifier)))

(defun list-to-action (e)
  (cond
   ((atom (car e))
    (cond
     ((eq (car e) (quote quote))
      '*quote)
     ((eq (car e) (quote lambda))
      '*lambda)
     ((eq (car e) (quote cond))
      '*cond)
     (t '*application)))
   (t '*application)))



(defun *self-evaluating (e table)
            e)
;(set '*self-evaluating (lambda (e table) e))

(defun *quote (e table)
      (text-of-quotation e))

(defun text-of-quotation (l) 
  (second l))

(defun *identifier (e table)
  (lookup-in-table
   e table 'initial-table))


(defun initial-table (name)
;  (print '--initial-table--)
  ;  (print name)
  (cond
   ((eq name (quote t)) t)
   ((eq name (quote nil)) nil)
  (t (build 
      (quote primitive)
      name))))

(defun lookup-in-table (name table table-f)
;  (print '--lookup-in-table--)
;  (print name)
;  (print table)
;  (print table-f)
  (cond
   ((null table) (funcall table-f name));should there be a funcall here?
   (t (lookup-in-entry
       name
       (car table)
       (lambda (name)
         (lookup-in-table
          name
          (cdr table)
          table-f))))))



(defun lookup-in-entry (name entry entry-f)
;  (print '--lookup-in-entry--)
;  (print name)
;  (print entry)
  ;(print entryf)
  (lookup-in-entry-help
   name
   (first entry)
   (second entry)
   entry-f))

(defun lookup-in-entry-help (name names values entry-f)
;  (print '--lookup-in-entry-help)
;  (print name)
;  (print names)
;  (print values)
;  (print entry-f)
  (cond
   ((null names) (funcall entry-f name))
   ((eq (car names) name)
   (car values))
  (t (lookup-in-entry-help
      name
      (cdr names)
      (cdr values)
      entry-f))))

(defun *application (e table)
;  (print '--*application--)
;  (print e)
  (apply_
   (meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(defun evcon (lines table)
  (cond
   ((meaning
     (question-of (car lines)) table)
    (meaning
     (answer-of (car lines)) table))
   (t (evcon (cdr lines) table))))

(defun question-of (l)
  (first l))
    
(defun answer-of (l)
  (second l))

(defun *cond (e table)
  (evcon (cond-lines e) table))

(defun cond-lines (l) 
  (cdr l))


(defun apply_ (fun vals)
;  (print '--apply_--)
  (cond
   ((primitive? fun)
    (apply-primitive
     (second fun) vals))
   ((non-primitive? fun)
    (apply-closure
     (second fun) vals))))

(defun primitive? (l)
;  (print '--primitive?--)
;  (print l)
  (eq
   (first l)
   (quote primitive)))

(defun non-primitive? (l)
  (eq
   (first l)
   (quote non-primitive)))

(defun apply-primitive (name vals)
  (cond
   ((eq name (quote car))
    (car (first vals)))
   ((eq name (quote cdr))
    (cdr (first vals)))
   ((eq name (quote cons))
    (cons (first vals) (second vals)))
   ((eq name (quote eq))
    (eq (first vals) (second vals)))
   ((eq name (quote atom))
    (atom (first vals) ))   
   ((eq name (quote not))
    (not (first vals) ))   
   ((eq name (quote null))
    (null (first vals) ))   
   ((eq name (quote number))
    (numberp (first vals) ))   
   ((eq name (quote zero))
    (zero (first vals) ))      
   ((eq name (quote add1))
    (add1 (first vals) ) )  
   ((eq name (quote sub1))
    (sub1 (first vals) )) ))

(defun function-of (l) 
  (car l))

(defun arguments-of (l) 
  (cdr l))

(defun evlis (args table)
  (cond
   ((null args) (quote ()))
   (t (cons (meaning (car args) table)
            (evlis (cdr args) table)))))

(defun apply-closure (closure vals)
  ;(print '--apply-closure--)
  ;(print closure)
  ;(print vals)
  (meaning (body-of closure)
           (extend-table
            (new-entry
             (formals-of closure) vals)
            (table-of closure))))

(defun body-of (l)
  (third l))

(defun extend-table (a b)
  (cons a b))

(defun new-entry (a b)
  (build a b))

(defun formals-of (l)
  (second l))

(defun table-of (l)
  (first l))

(defun *lambda (e table)
  (build (quote non-primitive)
         (cons table (cdr e))))
    
;-----
; TESTING
;-----
(apply-closure '((((u v w)(1 2 3))((x y z)(4 5 6)))(x y)(cons z x)) '((a b c)(d e f)))

(funcall '*identifier 'coffee '(((COFFEE) (T)) ((KLATCH PARTY) (5 (6)))))


(meaning '(cons z x) '(((x y)((a b c)(d e f)))((u v w)(1 2 3))((x y z)(4 5 6))))

(evlis '(z x) '(((x y)((a b c)(d e f)))((u v w)(1 2 3))((x y z)(4 5 6))))

(*cond '(cond (coffee klatch)(t party)) '(((coffee)(t))((klatch party)(5 (6)))))


(meaning '(add1 1) '())
(meaning '(add1 1) (quote()))

(expression-to-action 'coffee)
(funcall '*identifier 'coffee '(((COFFEE) (T)) ((KLATCH PARTY) (5 (6)))))


(lookup-in-table 'coffee '(((COFFEE) (T)) ((KLATCH PARTY) (5 (6)))) 'initial-table)

;(meaning e3 '())
(meaning '(lambda (x) (cons x y)) '(((y z)((8) 9))))
;=> (NON-PRIMITIVE ((((Y Z) (# 9))) (X) (CONS X Y)))
;? #?
(expression-to-action '(lambda (x) (cons x y)));=> *lambda
(funcall '*lambda '(lambda (x) (cons x y)) '(((y z)((8) 9))))
;=> (NON-PRIMITIVE ((((Y Z) (# 9))) (X) (CONS X Y)))
;? #
(build (quote non-primitive)  (cons '(((y z)((8) 9))) (cdr '(lambda (x) (cons x y)))))
;=> (NON-PRIMITIVE ((((Y Z) (# 9))) (X) (CONS X Y)))
'(((y z)((8) 9)));=>(((Y Z) ((8) 9)))
(cdr '(lambda (x) (cons x y)));=>  ((X) (CONS X Y))
(cons '(((y z)((8) 9))) '());=> ((((Y Z) ((8) 9))))
(cons '(((y z)((8) 9))) '((X) (CONS X Y)));=>((((Y Z) ((8) 9))) (X) (CONS X Y))
(cons '(((y z)((8) 9))) (cdr '(lambda (x) (cons x y))))
;=> ((((Y Z) ((8) 9))) (X) (CONS X Y))

(build 'a (cons '(((y z)((8) 9))) (cdr '(lambda (x) (cons x y)))))
;=>(A ((((Y Z) (# 9))) (X) (CONS X Y)))
(build 'a '((((Y Z) ((8) 9))) (X) (CONS X Y)))
;=>(A ((((Y Z) (# 9))) (X) (CONS X Y)))
(build 'a (build 'b (build 'c (build 'd (build 'e (build 'f 'g))))))
;=> # not a real issue - just a display convenience like ... at the end of 10 items...



;(value e1)
;(value e2)
;(value e3)
;(value_ e4);not a real expression
;(value_ e5)
;(value e6)

(cond
 ((eq 1 2) 'non-normal)
 (t 'normality))

(cond ((eq 1 2) 'non-normal) (t 'normality))

(primitive? '(cond));=>NIL
(non-primitive? '(cond));=>NIL ... ?

(list-to-action '(cond))

;Question 10.1


(value_ '(eq 1 1));truth values
(value_ '(eq 1 2));truth values
(value_ '(cond ((eq 1 2) 'non-normality) (t 'normality)));truth values
(value_ '23);numbers
(value_ '(add1 1));quoted s-expression

;-------
; YAY!
;-------

;-----
;Question 10.2
;-----

(setf e1
       '((lambda (x)
          (cond
           ((atom x)(quote 'done))
           ((null x)(quote 'almost))
           (t (quote 'never))))
         (quote (add1 1)))) ; where '(add1 1) is the inserted s-expr

(setf e1
       '((lambda (x)
          (cond
           ((atom x)(quote 'done))
           ((null x)(quote 'almost))
           (t (quote 'never))))
           (quote x))) ; where '(add1 1) is the inserted s-expr

(value_ e1)

(lookup-in-table 't '() 'initial-table)


;----
; 10.3
;----

(setf e2
     '(((lambda (x y)
         (lambda (u)
           (cond
            (funcall u x)
            (t y))))
       1 ())
       nil))

(defun apply-closure (closure vals)
  (print '--apply-closure--)
  (meaning (body-of closure)
           (extend-table
            (new-entry
             (formals-of closure) vals)
            (table-of closure))))

(value_ e2)
;--APPLY-CLOSURE-- 
;--APPLY-CLOSURE-- 
;NIL
;Two closures are applied

;-----
; YAY!
;-----
;10.4
(setf e3
     '((lambda (x)
        ((lambda (x)
           (add1 x))
         (add1 4)))
       6))

(value_ e3);=> 6
; Not correct? Not passing in value?
;Actually fine - its doing 4 + 1 = 5 then passing that to add1 -  the final 6 is ignored
;from the first lambda - you can change the 6 to -1 and get the same value
;------
; YAY!
;-----

;stepping through
(meaning e3 '())
(expression-to-action e3)
(*application e3 '())
;  (apply_
;   (meaning (function-of e) table)
;   (evlis (arguments-of e) table)))
(meaning (function-of e3) '());=> (NON-PRIMITIVE (NIL (X) ((LAMBDA (X) (ADD1 X)) (ADD1 4))))
(evlis (arguments-of e3) '());=> (6)
(apply_ '(NON-PRIMITIVE (NIL (X) ((LAMBDA (X) (ADD1 X)) (ADD1 4)))) '(6));=> 6
;? Should the nil be at the front?
(function-of e3);=> (LAMBDA (X) ((LAMBDA (X) (ADD1 X)) (ADD1 4)))
(meaning '(LAMBDA (X) ((LAMBDA (X) (ADD1 X)) (ADD1 4))) '())
;=> (NON-PRIMITIVE (NIL (X) ((LAMBDA (X) (ADD1 X)) (ADD1 4))))
;???
(expression-to-action '(LAMBDA (X) ((LAMBDA (X) (ADD1 X)) (ADD1 4))) );=> *LAMBDA
(funcall '*LAMBDA '(LAMBDA (X) ((LAMBDA (X) (ADD1 X)) (ADD1 4))) '())
;=> (NON-PRIMITIVE (NIL (X) ((LAMBDA (X) (ADD1 X)) (ADD1 4)))) ?


;10.5
;Whoa....
(initial-table 'bob);=>(primitive bob)
;hint from 10.6 that initial-table needs to be represented as functions...

(defun initial-table (name)
  (cond
   ((eq name (quote t)) t)
   ((eq name (quote nil)) nil)
  (t (build 
      (quote primitive)
      name))))
(initial-table 'bob)
; possible to rewrite this to be a stream of functions
;
; or too spun out. 
;
; could be if it is a member of the initial table - then it is a primitive - otherwise no
; something like
;(defun initial-table (name)
;  (cond
;   ((eq name (quote t)) t)
;   ((eq name (quote nil)) nil)
;  (t (cons name '()))))
;(initial-table 'bob)

(lookup-in-entry 'coffee '((cheesecake)(coffee)) '(lambda (x) (print x)))

(expression-to-action '(LAMBDA (X) ((LAMBDA (X) (ADD1 X)) (ADD1 4))) );=> *LAMBDA
(funcall '*lambda '(LAMBDA (X) ((LAMBDA (X) (ADD1 X)) (ADD1 4))) '())
;suspicious....
(expression-to-action '(add1 1))
(funcall '*application '(add1 1) '())
(funcall '*application '(add1 1) (initial-table '()))
(function-of '(add1 1))
(meaning (function-of '(add1 1)) '())
(evlis (arguments-of '(add1 1)) '())
(apply 'add1 '(1))
(primitive? '(add1))


;1. 
(value_ '(add1 1)) ;=> 2
;2. -meaning
(meaning '(add1 1) '()) ;=> 2
;3. -meaning
(expression-to-action '(add1 1)) ;=> *application
;4.-meaning
(funcall '*application '(add1 1) '()) ;=> 2
;5 -application
;*application
;  (apply_
;   (meaning (function-of e) table)
;   (evlis (arguments-of e) table)))
(function-of '(add1 1)) ;=> ADD1
;6. -application
(meaning 'add1 '()) ;=> (primitive add1)
;7. -application
(arguments-of '(add1 1)) ;=> (1)
;8. -application
(evlis '(1) '()) ;=> (1)
;9. -application
(apply_ '(primitive add1) '(1)) ;=> 2


(value_ '((lambda (x) (add1 x)) 2))
(value_ e3)
;(e3)

(setf e3_
     '((lambda (x)
         (add1 4))
       6))

(value_ e3_) ;=> 5

(setf e3__
     '((lambda (x)
         (add1 x))
       6))

(value_ e3__) ;=> 7
;1. -value
(value_ '((lambda (x) (add1 x)) 6));=>7
;2. meaning
(meaning '((lambda (x) (add1 x)) 6) '());=>7
;3. -meaning
(expression-to-action '((lambda (x) (add1 x)) 6)) ;=> *application
;4.-meaning
(funcall '*application '((lambda (x) (add1 x)) 6) '()) ;=> 7
;5 -application
;*application
;  (apply_
;   (meaning (function-of e) table)
;   (evlis (arguments-of e) table)))
(function-of '((lambda (x) (add1 x)) 6)) ;=> (LAMBDA (X) (ADD1 X))
;6. -application
(meaning '(LAMBDA (X) (ADD1 X)) '()) ;=> (NON-PRIMITIVE (NIL (X) (ADD1 X)))
;7. -application
(arguments-of '((lambda (x) (add1 x)) 6)) ;=> (6)
;8. -application
(evlis '(6) '()) ;=> (6)
;9. -application
(apply_ '(NON-PRIMITIVE (NIL (X) (ADD1 X))) '(6)) ;=> 2


(meaning '(lambda)  '())
;1. Basically the goal is to rewrite the __apply__ function so that the primitive and 
;non-primitive tags are no longer needed
;2. The tags come from the __meaning__ function inside the __application__ function
;3. Need to rewrite the __initial-table__ function with the  __meaning__ function to stop adding primitive and non-primitive flags
;4. Need to rewrite __apply__ so that it simply applies 'non-primitive to lambda functions
;   and applies primitive to everything else
;5. And change __*lambda__ function not to return non-pritimive

;So __initial-table__ changes from

(defun initial-table (name)
  (cond
   ((eq name (quote t)) t)
   ((eq name (quote nil)) nil)
  (t (build 
      (quote primitive)
      name))))
;to
(defun initial-table5 (name)
  (cond
   ((eq name (quote t)) t)
   ((eq name (quote nil)) nil)
   (t (cons name '()))))

(initial-table 'bob)
(initial-table5 'bob)

;and lambda becomes
(defun *lambda (e table)
  (build (quote non-primitive)
         (cons table (cdr e))))

(defun *lambda5 (e table)
  (cons (cons table (cdr e)) '()))

(*lambda '(boris) '())
(*lambda5 '(boris) '())

(defun list-to-action5 (e)
  (cond
   ((atom (car e))
    (cond
     ((eq (car e) (quote quote))
      '*quote)
     ((eq (car e) (quote lambda))
      '*lambda5)
     ((eq (car e) (quote cond))
      '*cond)
     (t '*application5)))
   (t '*application5)))

(defun expression-to-action5 (e)
  (cond
   ((atom e) (atom-to-action5 e))
   (t (list-to-action5 e))))

(defun meaning5 (e table)
  (funcall (expression-to-action5 e) e table))

(defun value5 (e)
  (meaning5 e (quote ())))

(defun *identifier5 (e table)
  (lookup-in-table
   e table 'initial-table5))

(defun atom-to-action5 (e)
  (cond
   ((numberp e) '*self-evaluating)
   (t '*identifier5)))

(defun apply5 (fun vals)
;  (print '--apply_--)
  (cond
   ((not (eq 'lambda (car fun)))
    (apply-primitive
     (second fun) vals))
   ((eq 'lambda (car fun))
    (apply-closure
     (second fun) vals))))

(defun *application5 (e table)
;  (print '--*application--)
;  (print e)
  (apply5
   (meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(value5 '(add1 1))
(value5 '((lambda (x) (add1 x)) 6))

;-----
; OH YEAH
;----

;10.6
(defun extend-table (entry table)
  (cons entry table))
(extend-table '(a b) '())

(defun extend-table6 (entry table)
  (lambda (name)
    (cond
     ((member name (first entry))
      (pick (index name (first entry))
            (second entry)))
     (t (funcall table name)))))
(extend-table '(a b) '())
(extend-table6 '(a b) '())
;(funcall (extend-table6 '((a b)(c d)) '()) '((e f)(g h)))

;Working through use of extend table
;1. Value of a lambda
(value5 '((lambda (x) (add1 x)) 6))
;2. Calls apply-closure
(apply-closure '(NIL (X) (ADD1 X)) '(6))
;3 Calls extend-table with these values
(defun apply-closure (closure vals)
  (print '--apply-closure--)
  (print closure)
  (print vals)
  (meaning (body-of closure)
           (extend-table
            (new-entry
             (formals-of closure) vals)
            (table-of closure))))

;4. Formals-of simplifies to
(formals-of '(NIL (X) (ADD1 X))) ;=> (X)
;5. table-of simplifies to 
(table-of '(NIL (X) (ADD1 X))) ;=> NIL
;6. new entry becomes
(new-entry
 '(x) '(6)) ;=> ((X) (6))
;7. extend-table becomes
(extend-table '((X) (6)) NIL) ;=>(((X)(6)))
;So our new function gives
(extend-table6 '((X) (6)) NIL)
;Calling the closure gives
;(funcall (extend-table6 '((X) (6)) NIL) 1)

(defun apply-closure6 (closure vals)
  (print '--apply-closure--)
  (print closure)
  (print vals)
  (meaning6 (body-of closure)
           (extend-table
            (new-entry
             (formals-of closure) vals)
            (table-of closure))))

(defun meaning6 (e table)
  (funcall (expression-to-action e) e table))

(defun expression-to-action6 (e)
  (cond
   ((atom e) (atom-to-action6 e))
   (t (list-to-action e))));?????

(defun atom-to-action6 (e)
  (cond
   ((numberp e) '*self-evaluating)
   (t '*identifier6)))

(defun *identifier6 (e table)
  (lookup-in-table6
   e table 'initial-table))

(defun lookup-in-table6 (name table table-f)
;  (print '--lookup-in-table--)
;  (print name)
;  (print table)
;  (print table-f)
  (cond
   ((null table) (funcall table-f name))
   (t (lookup-in-entry
       name
       (car table)
       (lambda (name)
         (lookup-in-table6
          name
          ;(cdr table)
          (car (funcall (cdr table)));!!!
          table-f))))))

(defun value6 (e)
  (meaning6 e (quote ())))

(value6 '(add1 12))

;------
; YAY!
;------

;10.7 - Lambda verification

(defun *lambda? (x)
  (and
   (eq (car x) 'lambda)
   (eq (count-lat x) 3)))

(defun count-lat (vec)
  (cond
   ((null vec) 0)
   (t  (+ 1 (count-lat (cdr vec))))))



(setf e5 
  '(lambda (lat) (cons (quote lat) 'lat)))

(count-lat e5)

(setf e6
  '(lambda (lat (lyst)) a (quote b)))

(setf e2
     '(((lambda (x y)
         (lambda (u)
           (cond
            (funcall u x)
            (t y))))
       1 ())
       nil))

(*lambda? e5);T
(*lambda? e6);NIL
(*lambda? e2);NIL

;----
;YAY
;----

;10.8
;old definition
(defun *lambda (e table)
;  (print '--*lambda--)
;  (print e)
;  (print table)
  (build (quote non-primitive)
         (cons table (cdr e))))

;new definition
(defun *lambda8 (e table)
  (build
   (quote non-primitive)
   (lambda (vals)
     (meaning (body-of e)
              (extend-table
               (new-entry (formals-of e) vals)
               table)))))

;1. Find out what *lambda function is called with (adding print statements to lambda function
(value_ '((lambda (x) (add1 x)) 12))
;2. Try to see if arguments can be passed to function
(*lambda '(lambda (x) (add1 x)) '())
;3. Try passing arguments to new function
(*lambda8 '(lambda (x) (add1 x)) '())
(cdr (*lambda8 '(lambda (x) (add1 x)) '()))
(car (cdr (*lambda8 '(lambda (x) (add1 x)) '())))
(funcall (car (cdr (*lambda8 '(lambda (x) (add1 x)) '()))) '(12))
;4 So we need to modify what calls lambda to be like this
(defun meaning8 (e table)
  (funcall (expression-to-action e) e table))

(defun list-to-action8 (e)
  (cond
   ((atom (car e))
    (cond
     ((eq (car e) (quote quote))
      '*quote)
     ((eq (car e) (quote lambda))
      '*lambda8)
     ((eq (car e) (quote cond))
      '*cond)
     (t '*application)))
   (t '*application)))

(defun expression-to-action8 (e)
  (cond
   ((atom e) (atom-to-action e))
   (t (list-to-action8 e))))

(defun meaning8 (e table);funcall here?
;  (print '--meaning--)
  ;(print e)
  ;(print table)
  (funcall (expression-to-action8 e) e table))

(defun value8 (e)
  (meaning8 e (quote ())))

(value8 '(add1 1))
(value_ '((lambda (x) (add1 x)) 12))
(value5 '((lambda (x) (add1 x)) 12))
(value8 '((lambda (x) (add1 x)) 12))
; No extra changes required?
;-----
;YAY
;----

;10.9


(defun initial-table (name)
;  (print '--initial-table--)
  ;  (print name)
  (cond ((eq name 'add1) (print 'buildingAdd1)))
  (cond
   ((eq name (quote t)) t)
   ((eq name (quote nil)) nil)
  (t (build 
      (quote primitive)
      name))))

(value_ e3)
;inserted print statement in 
;gets called twice

(defun initial-table9 (add1)
  (lambda (name)
    (cond
     ((eq name (quote t)) t)
     ((eq name (quote NIL)) NIL)
     ((eq name (quote add1)) add1)
     (t (build (quote primitive) name))))
  (build (quote primitive) add1))

(defun *identifier9 (e table)
  (lookup-in-table
   e table 'initial-table9))

(defun atom-to-action9 (e)
  (cond
   ((numberp e) '*self-evaluating)
   (t '*identifier9)))

(defun expression-to-action9 (e)
  (cond
   ((atom e) (atom-to-action9 e))
   (t (list-to-action e))))

(defun meaning9 (e table)
  (funcall (expression-to-action9 e) e table))

(defun value9 (e)
  (meaning9 e (quote ())))

(value9 e3)
;'primitive doesn't get called

(defun initial-table9 (add1)
  (lambda (name)
    (cond
     ((eq name (quote t)) t)
     ((eq name (quote NIL)) NIL)
     ((eq name (quote add1)) add1)
     ((eq name (quote quote)) *quote)
     ((eq name (quote identifier)) *identifier)
     ((eq name (quote lambda)) *lambda)
     ((eq name (quote cond)) *cond)
     ((eq name (quote application)) *application)
     (t (build (quote primitive) name))))
  (build (quote primitive) add1))

;-----
; YAY
;------
;10.10
(defun *if (e table)
      (if (meaning (test-pt e) table)
          (meaning (then-pt e) table)
        (meaning (else-pt e) table)))

(defun test-pt (list)
  (second list))

(defun then-pt (list)
  (third list))

(defun else-pt (list)
  (fourth list))

(fourth (list 1 2 3 4))

(test-pt '(add1 1))

(defun list-to-action10 (e)
  (cond
   ((atom (car e))
    (cond
     ((eq (car e) (quote quote))
      '*quote)
     ((eq (car e) (quote lambda))
      '*lambda)
     ((eq (car e) (quote if))
      '*if)
     (t '*application)))
   (t '*application)))

(defun expression-to-action10 (e)
  (cond
   ((atom e) (atom-to-action e))
   (t (list-to-action10 e))))

(defun meaning10 (e table)
  (funcall (expression-to-action10 e) e table))

(defun value10 (e)
  (meaning10 e (quote ())))

(if (eq 1 2) ('nonNormal) )
(if (eq 1 2) ('nonNormal) 'normal)

(value10 '(if (eq 1 2) ('nonNormal) 'normal))















