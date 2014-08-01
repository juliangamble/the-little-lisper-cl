;The Little Lisper - Chapter 5

(setf x 'comma)
(setf y 'dot)
(setf a 'kiwis)
(setf b 'plums)
(setf lat1 '(bananas kiwis))
(setf lat2 '(peaches apples bananas))
(setf lat3 '(kiwis pears plums bananas cherries))
(setf lat4 '(kiwis mangoes kiwis guavas kiwis))
(setf l1 '((curry) () (chicken) ()))
(setf l2 '((peaches) (and cream)))
(setf l4 '())

;5.1

(defun subst-sauce (a lat)
  (cond
   ((null lat) '())
   (t (cond
       ((eq (car lat) 'sauce)
        (cons a (cdr lat)))
       (t (cons (car lat)
                (subst-sauce
                 a (cdr lat))))))))

(setf a1 'chili)
(setf a4 'sauce)
(setf l4 '(texas hot chili))
(setf l5 '(soy sauce and tomato sauce))
(setf l2 '())

(subst-sauce a1 l4);(texas hot chili);correct
(subst-sauce a1 l5);(soy chili and tomato sauce);correct
(subst-sauce a4 l2);NIL;correct

(defun multisubst-kiwis (a lat)
  (cond
   ((null lat) '())
   (t (cond
       ((eq (car lat) 'kiwis)
        ;(cons a (cdr lat)))
       (multisubst-kiwis a (cons a (cdr lat))))
       (t (cons (car lat)
                (multisubst-kiwis
                 a (cdr lat))))))))

(multisubst-kiwis  b lat1);(bananas plums);correct
(multisubst-kiwis  y lat2);(peaches apples bananas);correct
(multisubst-kiwis  y lat4);(dot mangoes dot guavas dot);correct
(multisubst-kiwis  y l4);NIL;correct

;5.2 
(defun subst-sauce (a lat)
  (cond
   ((null lat) '())
   (t (cond
       ((eq (car lat) 'sauce)
        (cons a (cdr lat)))
       (t (cons (car lat)
                (subst-sauce
                 a (cdr lat))))))))

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

(setf new1 'vanilla)
(setf o11 'chocolate)
(setf o21 'banana)
(setf mylat '(banana ice cream with chocolate topping))

(subst2 new1 o11 o21 mylat);(vanilla ice cream with chocolate topping);correct

(defun multisubst2 (new o1 o2 lat)
  (cond
  ((null lat) '())
  (t (cond 
      ((eq (car lat) o1)
       ;(cons new (cdr lat)))
       (multisubst2 new o1 o2 (cons new (cdr lat))))
      ((eq (car lat) o2)
       ;(cons new (cdr lat)))
       (multisubst2 new o1 o2 (cons new (cdr lat))))
      (t (cons (car lat)
               (multisubst2 new 
                            o1 o2 (cdr lat))))))))

(multisubst2 x a b lat1);(bananas comma);correct
(multisubst2 y a b lat3);(dot pears dot bananas gerries);correct
(multisubst2 a x y lat1);(bananas kiwis);correct

;5.3
(atom 'bob)
(quote 'bob)
(quote (quote 'bob))
(list 'bob)

(defun multidown (lat)
  (cond
   ((null lat) '())
   (t ;(cond
       ;((atom (cat lat))
        (cons (list (car lat)) (multidown  (cdr lat))))))

(multidown lat1);((BANANAS) (KIWIS));correct
(multidown lat2);((PEACHES) (APPLES) (BANANAS));correct
(multidown l4);(); correct

;5.4

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

(occurN (list 'bananas) (list 'bananas 'peaches))
(occurN lat1 l4)
(occurN lat1 lat2)
(occurN lat1 lat3)

;5.4

(defun I_ (a_ lat2_)
  (cond
   ((null a_) '())
   ((null lat2_) '())
   (t (cond
       ((eq (car lat2_) a_)
        a_)
       (t (I_ a_ (cdr lat2_)))))))

(I_ 'bananas (list 'bananas 'peaches))

(defun I_exists (a_ lat2_)
  (cond
   ((null a_) '())
   ((null lat2_) '())
   (t (cond
       ((eq (car lat2_) a_)
        t)
       (t (I_ a_ (cdr lat2_)))))))

(I_exists 'bananas (list 'bananas 'peaches))

(defun I (lat1_ lat2_)
  (cond
   ((null lat1_) '())
   ((null lat2_) '())
  (t (cond
      ((I_exists (car lat2_) lat1_)
       (I_ (car lat2_) lat1_))
      (t (I (cdr lat2_) lat1_))))))

(I (list  'apples 'bananas) (list 'bananas 'peaches))
(I (list  'apples 'bananas 'pears) (list 'bananas 'peaches 'pears))

(defun multiI (lat1_ lat2_)
  (cond
      ((null lat1_) '())
   ((null lat2_) '())
   (t (cond
       ((I_exists (car lat2_) lat1_)
        (cons (car lat2_) (multiI (cdr lat2_) lat1_)))
       (t (multiI (cdr lat2_) lat1_))))))

(multiI (list  'apples 'bananas 'pears) (list 'bananas 'peaches 'pears))

(I lat1 l4);NIL;correct
(I lat1 lat2);bananas;correct
(I lat1 lat3);kiwis
(multiI lat1 l4);NIL;correct
(multiI lat1 lat2);bananas;correct
(multiI lat1 lat3);(kiwis bananas);correct

;5.6
;Commandment one - always ask null? as the first question in expressing any function.
;This is not done here

;5.7
;Because the changing argument should be tested in the terminating condition - and in this case it is not
; In this case the other argument is tested as the terminating condition - which then triggers a test
; of the recurring agument 

(defun zero (n)
  (= 0 n))

(defun sub1 (n)
  (- n 1))

(defun =_ (n m)
  (cond
   ((zero n) (zero m))
   (t (= n (sub1 m)))))

(=_ 1 2)

; In the default implementation - this implementation doesn't actually work - we assume they meant this:
(defun =_ (n m)
  (cond
   ((zero n) (zero m))
   (t (= m (sub1 n)))))

(=_ 1 2)

;In this case - the recurring argument is not directly tested for a null condition, but it is triggered when
;recurring as the arguments are flipped around - in which case it is in the spirit of the the sixth commandment


;5.8
;Incorrect version
(defun count0 (vec)
  (cond
   ((null vec) 1)
   (t (cond 
       ((zero (car vec))
        (cons 0 (count0 (cdr vec))))
       (t (count0 (cdr vec)))))))

(count0 (list 0 0 0 0))

;correct version
(defun count0 (vec)
  (cond
   ((null vec) 0)
   (t (cond 
       ((zero (car vec))
        (+ 1 (count0 (cdr vec))))
       (t (count0 (cdr vec)))))))

(count0 (list 0 0 0 0))

;5.9
(not (atom (list a)))

(defun listlength (lat)
  (cond
   ((null lat) 0)
   ((atom lat) 0)
   (t (+ 1 (listlength (cdr lat))))))

(listlength (list 'a 'b 'c 'd 'e))
(listlength '())

(defun up (l)
  (cond
   ((null l) '())
   (t (cond
       ((eq 1 (listlength (car l)))
        (cons (car (car l)) (cdr l)))
       (t (cons (car l) (up (cdr l))))))))

(up (list 'a 'b (list 'c) 'd 'e))

(defun multiup (l)
  (cond
   ((null l) '())
   (t (cond
       ((eq 0 (listlength (car l)))
        (multiup (cdr l)))
       ((eq 1 (listlength (car l)))
        (cons (car (car l)) (multiup (cdr l))))
       (t (cons (car l) (multiup (cdr l))))))))

(multiup (list 'a 'b (list 'c) 'd (list 'e)))

(multiup l4);NIL;correct
(multiup l1);(CURRY CHICKEN);correct
(multiup l2);(PEACHES (AND CREAM));correct

;5.10
;Check all the functions in 4 and 5 to see if they obey the Commandments. 
;Some don't all obey the forth commandment - to ask two questions - but they're numeric
;so they're in the spirit - or the test is combined
;They do all appear to obey the sixth commandment - testing for termination on the value changed during recursion
