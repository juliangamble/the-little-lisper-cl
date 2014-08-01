;The Little Lisper Chapter 8

(setf r1 '((a b)(a a)(b b)))
(setf r2 '((c c)))
(setf r3 '((a c)(b c)))
(setf r4 '((a b)(b a)))
(setf f1 '((a 1)(b 2)(c 2)(d 1)))
(setf f2 '())
(setf f3 '((a 2)(b 1)))
(setf f4 '((1 $)(3 *)))
(setf d1 '(a b)) 
(setf d2 '(c d))
(setf x 'a)


      

;8.1
(defun member? (a lat)
  (cond
   ((null lat) NIL)
   (t (or
       (eq (car lat) a)
       (member? a (cdr lat))))))

(defun makeset (lat)
  (cond 
   ((null lat) '())
   ((member? (car lat) (cdr lat))
    (makeset (cdr lat)))
   (t (cons (car lat)
            (makeset (cdr lat))))))

(makeset '(a b c d e d c b a))

(defun flatten (lat acc)
  (cond
   ((null lat) acc)
   ((notatom (car lat))
    (flatten (car lat) (flatten (cdr lat) acc)))
   (t (flatten (cdr lat) (cons (car lat) acc)))))

(flatten '(a b c (d e f)) '())
(flatten r3 '())
(flatten '((a c)(b c)) '())

(defun domset (rel)
  (cond
   ((null rel) '())
   (t (makeset (flatten rel '())))))

(domset r1);(a b);correct
(domset r2);(c);correct
(domset r3);(a c v);correct

(defun build (a b)
  (cons a (cons b '())))

(build 'a 'b)
  
(defun idrel (s)
  (cond
   ((null s) '())
   (t (cons (build (car s) (car s))
            (idrel (cdr s))))))

(idrel '(a b c))

;8.2
(idrel (domset r1))
(idrel (domset r2))
(idrel (domset r3))

(member? 'a '(a b c))

(defun first_ (l)
  (cond
   ((null l) '())
   (t (car l))))

(defun second_ (l)
  (cond
   ((null l) '())
   (t (car (cdr l)))))

(defun third_ (l)
  (cond
   ((null l) '())
   (t (car (cdr (cdr l))))))

(first_ '(a b))
(second_ '(c d))
(third_ '(a b c))

(defun pair? (lat)
  (cond
   ((null lat) NIL)
   ((atom lat) NIL)
   ((and (and (not (eq (first_ lat) NIL))
              (not (eq (second_ lat) NIL))))
    (eq (third lat) NIL))
   (t NIL)))

(pair? '(a b))
(pair? 'a)
(pair? '(a b c))

(defun rel? (rel)
  (cond
   ((null rel) t)
   ((atom rel) NIL)
   ((pair? (car rel))
    (rel? (cdr rel)))
   (t NIL)))

(rel? '((a b)))
(rel? '((a a)(a b)))
(rel? '(a b))
(rel? '((a b c)))
(rel? '((a b) c))

(defun eq-pair (pair-a pair-b)
  (cond
   ((null pair-a) NIL)
   ((null pair-b) NIL)
   ((atom pair-a) NIL)
   ((atom pair-b) NIL)
   ((not (pair? pair-a)) NIL)
   ((not (pair? pair-b)) NIL)
   ((and (eq (first_ pair-a) 
             (first_ pair-b))
         (eq (second_ pair-a)
             (second_ pair-b))))
   (t NIL)))

(eq-pair '(a b) '(a b))
(eq-pair '(a a) '(b b))
(eq-pair '(a b c) '(a b c))
(eq-pair '(a a) 'a)
   
(defun member-pair? (pair rel)
  (cond
   ((null pair) t)
   ((null rel) NIL)
   ((not(pair? pair)) NIL)
   ((not(rel? rel)) NIL)
   ((eq-pair (car rel) pair) t)   
   (t (member-pair? pair (cdr rel)))))
   
(member-pair? '(a b) '(a b c))
(member-pair? '(a d) '(a b c))
(member-pair? '(a a) '((a a)(a b)))
(member-pair? '(a a) '((a b)(a a)))

(defun member-rel? (rel1 rel2)
  (cond
   ((null rel1) t)
   ((null rel2) NIL)
   ((not (rel? rel1)) NIL)
   ((not (rel? rel2)) NIL)
   ((member-pair? (car rel1) rel2)    
    (member-rel? (cdr rel1) rel2))
   (t NIL)))

(member-rel? '((a a)) '((a b)(a a)))
(member-rel? '((a a)) '((a b)(a c)))
(member-rel? '((a)) '((a b)(a a)))
(member-rel? '((a)) '((a b)(a)))
(member-rel? '(a a) '((a b)(a a)))
(member-rel? '((a a)) '(a b))

(defun reflexive? (lat)
  (cond
   ((null lat) NIL)
   (t (member-rel? (idrel (domset lat)) lat))))

(domset r1)
(idrel (domset r1))
(member-rel? (idrel (domset r1)) r1)

(reflexive? r1);T;correct
(reflexive? r2);T;correct
(reflexive? r3);NIL;correct

;8.3
;(defun member* (a l)
;  (cond
;   ((null l) NIL)
;   ((atom a)
;    (cond     
;     ((atom (car l))
;      (or
;       (eq (car l) a)
;       (member* a (cdr l))))
;     (t (or 
;         (member* a (car l))
;         (member* a (cdr l))))))
;   (t (or
;       (and
;        (member* (car a) (car l))
;        (member* (cdr (car a)) (car l)))
;       (member* (cdr a) (cdr l))))))

(defun member* (a l)
  (cond
   ((null l) NIL)
   ((atom (car l))
    (or
     (eq (car l) a)
     (member* a (cdr l))))
   (t (or 
       (member* a (car l))
       (member* a (cdr l))))))

(member* 'chips '((potato) (chips ((with) fish) (chips))))

(defun lat? (l)
  (cond
   ((null l) t)
   ((atom (car l)) (lat? (cdr l)))
   (t nil)))
(lat? '(bacon (and eggs)));NIL;correct
(lat? '(bacon and eggs));T;correct

(defun eqlat? (lat1 lat2)
  (cond
   ((and
    (null lat1)
     (null lat2)) t)
   ((null lat1) NIL)
   ((null lat2) NIL)
   ((not (lat? lat1)) NIL)
   ((not (lat? lat2)) NIL)
   ((eq (car lat1) (car lat2))
        (eqlat? (cdr lat1) (cdr lat2)))
   (t NIL)))

(eqlat? '(a b) '(a b));T
(eqlat? '(a b) '(a a));F
(eqlat? '(a b) '(a b c));F
(eqlat? '(a b) '(a (b)));F

(defun non-atom? (a)
  (not (atom a)))

(non-atom? 'a)
(non-atom? '(a))

(defun equal? (s1 s2)
  (cond
   ((and (atom s1) (atom s2))
        (eq s1 s2))
   ((and
     (non-atom? s1)
     (non-atom? s2))
    (eqlist? s1 s2))
    (t NIL)))

(equal? 'a 'a)
(equal? '(a) '(a))
  
(defun eqlist? (l1 l2)
  (cond
   ((and (null l1) (null l2)) t)
   ((or (null l1) (null l2)) NIL)
   (t (and
       (equal? (car l1) (car l2))
       (eqlist? (cdr l1) (cdr l2))))))

(eqlist? '((a b)(c d)) '((a b)(c d)))
(eqlist? '((a b)(c d)) '((a b)(c e)))
                

;based on tailp function in CL
;(defun member_ (lista listb)
;  (cond
;   ((null lista) t)
;   ((null listb) NIL)
;   ((atom listb) NIL);?
;   ((atom lista)
;    (member* lista listB))
;   ((lat? lista)
;    (cond
;     ((lat? listb)
;      (eqlat? lista listb))
;     (t (or
;      (member_ lista (car listb))
;         (member_ lista (cdr listb))))))
;   (t (or
;       (member_ (car lista) listb)
;       (member_ (cdr lista) listb)))))    

(defun member_ (lista listb)
  (cond
   ((null lista) t)
   ((null listb) NIL)
   ((atom listb)
    (eq lista listb))
   ((atom lista)
    (member* lista listb))
   ((eqlist? lista listb) t)
   (t (or (member_ lista (car listb))
       (member_ lista (cdr listb))))))
   ;(t NIL)))
                
(member_ '((with) fish) '((potato) (chips ((with) fish) (chips))));T
(member_ '(chips) '((potato) (chips ((with) fish) (chips))));T
(member_ '((a a)) '((a b)(a a)));T
(member_ '((a c)) '((a b)(a a)));?
(member_ '(a a) '((a b)(a a)))


(defun subset? (set1 set2)
  (cond
   ((null set1) t)
   ((member_ (car set1) set2)
    (subset? (cdr set1) set2))
   (t NIL)))

(subset '(b) '(c b))
(subset '((a b)) '((a a)(a b)))
(subset '((a a)(a b)) '((a a)(a b)(a c)(a d)))

(defun eqset? (set1 set2)
  (and
   (subset? set1 set2)
   (subset? set2 set1)))

(eqset? '((a b)) '((a b)))

(defun revrel (rel)
  (cond
   ((null rel) '())
    (t (cons 
       (build
        (second (car rel))
        (first (car rel)))
       (revrel (cdr rel))))))

(revrel '((a b)(c d)))

(defun symmetric? (rel)
  (eqset? rel (revrel rel)))

(revrel r1)

(symmetric? r1);NIL;correct
(symmetric? r2);T;correct
(symmetric? f2);T;correct

(defun intersect (set1 set2)
  (cond
   ((null set1) '())
   ((not (member_ (car set1) set2))
    (intersect (cdr set1) set2))
   (t (cons (car set1)
            (intersect (cdr set1) set2)))))
  
(intersect '((a a)(a b)(b b)) '((a a)(b b)))
(intersect '(a b) '(a b c))

(defun antisymetric? (rel)
  (subset? (intersect (revrel rel) rel) (idrel (domset rel))))

(revrel r1)
(intersect (revrel r1) r1)
(idrel (domset rel)
  
(antisymetric? r1);T;correct
(antisymetric? r2);T;correct
(antisymetric? r4);NIL;correct

(defun asymmetric? (rel)
  (null (intersect rel (revrel rel))))

(asymmetric? r1);NIL
(asymmetric? r2);NIL
(asymmetric? r3);T
(asymmetric? r4);NIL
(asymmetric? f1);T
(asymmetric? f2);T
(asymmetric? f3);T
(asymmetric? f4);T
(asymmetric? d1);error
(asymmetric? d2);error
(asymmetric? x);error

;assymetric - contains no matching pairs in the relation
; For all a and b in X, if a is related to b, then b is not related to a.

;8.4
(defun fapply (f x)
  (cond
   ((null f) NIL)
   ((null x) NIL)
   ((and (rel? f) (atom x))
    (cond
     ((eq (first (car f)) x) (second (car f)))
     (t (fapply (cdr f) x))))
   (t NIL)))

(fapply '((a 1)(b 2)) 'b)
(fapply f1 'x);book incorrect - presume they mean 'a
(fapply f2 'x)
(fapply f3 'x)
(fapply f1 'a);1;correct
(fapply f2 'a);NIL;correct
(fapply f3 'a);2;correct

;8.5
(defun fcomp-pair (rel1 pair)
  (cond
   ((null rel1) '())
   ((null pair) NIL)
   ((and (rel? rel1) (pair? pair))
    (cond 
     ((not (null (fapply rel1 (second pair))))
      (build (first pair) (fapply rel1 (second pair))))
     (t NIL)))
   (t NIL)))

(fcomp-pair '((a b)(c d)) '(a c));(a d);correct
(fcomp-pair '((a b)(c d)) '(a b));NIL;correct

(defun fcomp (rel1 rel2)
  (cond
   ((null rel1) '())
   ((null rel2) NIL)
   ((and (rel? rel1) (rel? rel2))
    (cond
     ((not (null (fapply rel1 (second (car rel2)) )))
      (cons (fcomp-pair rel1 (car rel2))
            (fcomp rel1 (cdr rel2))))
     (t (fcomp rel1 (cdr rel2)))))
   (t NIL)))

(defun fcomp (rel1 rel2)
  (cond
   ((null rel1) '())
   ((null rel2) NIL)
   ((and (rel? rel1) (rel? rel2))
    (cond
     ((rel? (cdr rel))
      (cons
       (cond
        ((not (null (fapply rel1 (second (car rel2)) )))
         (build (first (car rel2)) (fapply rel1 (second (car rel2)) )))
        (t NIL))
       (fcomp rel1 (cdr rel2))))
     (t
      (cond
        ((not (null (fapply rel1 (second (car rel2)) )))
         (build (first (car rel2)) (fapply rel1 (second (car rel2)) )))
        (t NIL)))))
   (t NIL)))

(second (car '((b c)(d e))))
(fapply  '((a b)(c d)) 'c)
(fapply '((a b)(c d)) (second (car '((b c)(d e)))))

(fcomp '((a b)(c d)) '((b c)(d e)))
(fcomp f1 f4);NIL;correct
(fcomp f1 f3);NIL;correct
(fcomp f4 f1);((A $) (D $));correct
(fcomp f4 f3);((B $));correct

(cons NIL '())
'()
(cons '(b d) NIL)
(cons '(b d) '())
(cons '(b d) (cons '() '()))
(cons '(b d) (cons NIL '()))
(cons '(b d) (cons NIL NIL))
(cons '(b d) (cons '() NIL))

;8.6
(defun rapply (rel x)
  (cond
   ((null rel) '())
   ((null x) NIL)
   ((and (rel? rel) (atom x))
    (cond
     ((eq (first (car rel)) x) 
      (cons (second (car rel)) (rapply (cdr rel) x)))
     (t (rapply (cdr rel) x))))
   (t NIL)))

(rapply '((a 1)(b 2)) 'b)
(rapply f1 x);1;correct
(rapply r1 x);(b a);correct
(rapply f2 x);NIL;correct

;8.7
(defun rin (x set)
  (cond
   ((null x) NIL)
   ((null set) '())
   ((lat? set)
    (cons (build x (car set))
          (rin x (cdr set))))
   (t NIL)))

(rin 'a d1);((A A) (A B));correct - note typo in notes - said x when they meant 'a
(rin 'a d2);((A C) (A D));correct - note typo in notes - said x when they meant 'a
(rin 'a f2);NIL;correct

;8.8
(defun union_ (set1 set2)
  (cond
   ((null set1) set2)
   ((member_ (car set1) set2)
    (union (cdr set1) set2))
   (t (cons (car set1)
            (union (cdr set1) set2)))))

(union_ '(tomatoes and marcaroni casserole) '(marcaroni and cheese))

(defun rcomp (rel1 rel2)
  (cond
   ((null rel1) '())
   (t (union
       (rin
        (first (car rel1))
        (rapply rel2 (second (car rel1))))
       (rcomp (cdr rel1) rel2)))))

(rcomp '((a b)(b d)) '((a b)(b d)));((a d))
(rcomp '((a b)(b d)) '((a b)(b d)));((a d))

(rcomp r1 r3);((A C) (A C) (B C))
(rcomp r1 f1);((A 2) (A 1) (B 2))
(rcomp r1 r1);((A B) (A A) (A B) (B B))

;8.9
(defun transitive? (rel)
  (subset? (rcomp rel rel) rel))

;(defun transitive? (rel)
;  (cond
;   ((not (null (rcomp rel rel)))
;    (subset? (rcomp rel rel) rel))
;   (t NIL)))
; incorrect

(transitive? r1);T;correct
(transitive? r3);T;correct
(transitive? f1);T;correct

;non transitive relation
(transitive? '((a b)(b d)))
;ie transforms to not nil and not something that is part of the parent relation

;8.10
(defun quasi-order? (rel)
  (and (reflexive? rel) (transitive? rel)))

(quasi-order? r1);T

(defun partial-order? (rel)
  (and (quasi-order? rel) (antisymetric? rel)))

(partial-order? r1);T

(defun equivalence? (rel)
  (and (quasi-order? rel) (symmetric? rel)))

(equivalence? r1);NIL








    
   
   







