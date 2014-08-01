;The Little Lisper Chapter 6

(setf l1 '((fried potatoes)(baked (fried)) tomatoes))
(setf l2 '(((chili) chili (chili))))
(setf l3 '())
(setf lat1 '(chili and hot)) 
(setf lat2 '(baked fried)) 
(setf a 'fried)


;6,1
(atom 'chili)
(atom '(chili))
(atom (car '(chili)))
(consp (car l2))

(defun notatom (lat)
  (not (atom lat)))

(notatom 'a)
(notatom '(a))

(defun down* (lat)
  (cond
   ((null lat) '())
   ((notatom (car lat))
    (cons (down* (car lat))
            (down* (cdr lat))))
   (t (cons (list (car lat))
            (down* (cdr lat))))))

(down* (list 'bob))
(down* l2);
(down* l3);NIL;correct
(down* lat1);((CHILI) (AND) (HOT));correct 

;6.2
(defun add1 (n)
  (cond
   ((null n) '())
   ((+ n 1))))

(add1 1)

(defun occurNa* (a1 lat)
  (cond
   ((null lat) 0)
   ((null a1) 0)
   (t (cond
       ((notatom (car lat))
        (+ (occurNa* a1 (car lat))
           (occurNa* a1 (cdr lat))))
       ((eq (car lat) a1)
        (add1 (occurNa* a1 (cdr lat))))
       (t (occurNa* a1 (cdr lat)))))))

(occurNa* 'c (list 'a 'b 'c))
(occurNa* 'bananas '(((bananas))(bananas)))

(defun occurN* (alat lat)
  (cond 
   ((null alat) 0)
   ((null lat) 0)
   ((notatom (car alat))
    (+ (occurN* (car alat) lat)
       (occurN* (cdr alat) lat)))
   (t (+ (occurNa* (car alat) lat)
         (occurN* (cdr alat)  lat)))))

(occurN* lat1 l2);3;correct
(occurN* '(baked fried) '((fried potatoes)(baked (fried)) tomatoes))
(occurN* lat2 l1);3;correct
(occurN* lat1 l3);0;correct

;6.3

(defun double (a lat)
  (cond
   ((null lat) '())
   ((eq (car lat) a)(cons a lat))
   (t (cons (car lat)
            (double a (cdr lat))))))

(double 'bob '(bob the builder))

(defun double* (a lat)
  (cond
   ((null lat) '())
   ((notatom (car lat))
    (cons (double* a (car lat))
          (double* a (cdr lat))))
   ((eq (car lat) a)(cons a lat))
   (t (cons (car lat)
            (double* a (cdr lat))))))

(double* 'bob '(bob the builder))
(double* 'bob '((bob) the builder))
(double* a l1);((FRIED FRIED POTATOES) (BAKED (FRIED FRIED)) TOMATOES);correct
(double* a l2);(((CHILI) CHILI (CHILI)));correct
(double* a lat2);(BAKED FRIED FRIED);correct

;6;4
;Function lat? from Chapter 2
(defun lat? (l)
  (cond
   ((null l) t)
   ((atom (car l)) (lat? (cdr l)))
   (t nil)))
(lat? '(bacon (and eggs)));NIL;correct
(lat? '(bacon and eggs));T;correct
;Why does it have to ask three questions? (and not two like other functions in chapter 2)
;;The only other real function is member - which determines if a flat-list contains an atom or not
;; The first question is the termination of the recursion on the list which is being examined
;; The second question is the truth test - does this list contain atoms only
;; The third question is if this list contains a non-atom (ie a list) - then fail
;;; So the reason we ask three questions is because the definition of lat? is two-fold 
;;; (both contains atoms and doesn't contain lists)
;Why does lat not have to recur on the car?
;;Because lat? is asking the questions whether this is a 'flat list' ie "Is this is a list but not a list of lists."

;6.5
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
(member* '((with) fish) '((potato) (chips ((with) fish) (chips))));?
(member* '((a a)) '((a b)(a a)))
(member* '(a a) '((a b)(a a)))


(defun notatom (lat)
  (not (atom lat)))

;version that starts at the end of the list
(defun member-backwards* (a l oldl)
  (cond
   ((null l) (member* a oldl))
   ((notatom (car l))
             (or (member-backwards* a (cdr l) oldl)
                   (member-backwards* a (car l) oldl)))
   (t 
    (member-backwards* a 
                       (cdr l) 
                       (cons (car l) oldl)))))

(member-backwards* 'chips '((potato) (chips ((with) fish) (chips))) '())
                                     
;6.6
(defun addvec (vec)
  (cond 
   ((null vec) 0)
   (t (+ (car vec)(addvec (cdr vec))))))

(addvec '(1 2 3))

(defun list+ (vec)
  (cond 
   ((null vec) 0)
   ((notatom (car vec))
    (+ (list+ (car vec))
       (list+ (cdr vec))))
   (t (+ (car vec)(list+ (cdr vec))))))

(list+ '(1 2 3))
(list+ '(1 (2 (3))))

;6.7
(defun g* (lvec acc)
  (cond
   ((null lvec) acc)
   ((atom (car lvec))
    (g* (cdr lvec)(+ (car lvec) acc)))
   (t (g* (car lvec)(g* (cdr lvec) acc)))))

(g* '(1 (2 (3))) 0)
;'This takes a list of numbers and adds them into the accumulator (acc)

;6.8
(defun f* (l acc)
  (cond 
   ((null l) acc)
   ((atom (car l))
    (cond
     ((member (car l) acc) (f* (cdr l) acc))
     (t (f* (cdr l)(cons (car l) acc)))))
   (t (f* (car l)(f* (cdr l) acc)))))

(f* '(1 2 3 4 (4 5 3 2 1)) '())
;This removes duplicates and returns list in reverse order without sublists

;6.9
(defun add1 (a)
  (+ 1 a))

(defun occur (a lat)
  (cond
   ((null lat) 0)
   (t (cond
       ((eq (car lat) a)
        (add1 (occur a (cdr lat))))
       (t (occur a (cdr lat)))))))

(occur 'a '(a b c d c (b a)))

(defun occur (a lat acc)
  (cond
   ((null lat) acc)
   (t (cond
       ((eq (car lat) a)
        (occur a (cdr lat) (add1 acc)))
       (t (occur a (cdr lat) acc))))))

(occur 'a '(a b c d c (b a)) 0)

(defun occur (a lat acc)
  (cond
   ((null lat) acc)
   (t (occur a (cdr lat) 
             (cond 
              ((eq (car lat) a)
               (add1 acc))
              (t acc))))))

(occur 'a '(a b c d c (b a)) 0)

;The original value for acc is 0

;6.10
(defun occur* (a lat acc)
  (cond 
   ((null lat) acc)
   ((notatom (car lat))
    (occur* a (cdr lat) (occur* a (car lat) acc)))
   (t (cond
       ((eq (car lat) a)
        (occur* a (cdr lat) (add1 acc)))
       (t (occur* a (cdr lat) acc))))))

(occur* 'a '(a b c d c (b a)) 0)






        





    
             










