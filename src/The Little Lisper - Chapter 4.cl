;The Little Lisper
;Chapter 4

(setf vec1 '(1 2))
(setf vec2 '(3 2 4))
(setf vec3 '(2 1 3))
(setf vec4 '(6 2 1))
(setf l '())
(setf zero 0)
(setf one 1)
(setf three 3)
(setf obj '(x y))


;4.1
(defun zero (n)
  (cond
   ((null n) '())
   ((= zero n) t)
   (t '())))

(zero 1)
(zero zero)
(zero 0)
(zero NIL)
;(zero ());not valid
;(zero (1 2));not valid

(- 3 1)

(defun sub1 (n)
  (cond
   ((null n) '())
   ((- n 1))))

(sub1 12)
(sub1 3)
;(sub1 'a)
(sub1 ())
(sub1 -12)

(defun duplicate (n obj)
  (cond
   ((zero n ) '())
   ((null obj) NIL)
   ((= 1 n) (cons obj '()))
   (t (cons obj
            (duplicate (sub1 n) obj)))))

(duplicate 3 obj);((X Y) (X Y) (X Y));correct
(duplicate zero obj);NIL;correct
(duplicate one vec1);(x y);correct

;4.2
(defun multvec (lat)
  (cond
   ((null lat) 1)
   (t (* (car lat)
         (multvec (cdr lat))))))

(multvec vec2);24;correct
(multvec vec3);6;correct
(multvec l);1;correct

;4.3 Fifth Commandment
;Similar to *, When building a value with ^, always use 1 for the value of the terminating line, 
;for raising to the power of 1 does not change the value of an exponentiation.

;4.4
;3^4 = 3^3 * 3
;3^3 = 3^2 * 3
;3^2 = 3^1 * 3
;3^1 = 3^0 * 3
;3^4 = 1 * 3 * 3 * 3 * 3
; = 81

;4.5
(defun add1 (n)
  (cond
   ((null n) '())
   ((+ n 1))))
(add1 1)

(defun index_ (a lat)
  (cond
   ((null a) '())
   ((null lat) '())
   ((eq a (car lat)) 1)
   (t (add1 
       (index_ a (cdr lat))))))

(setf a_ 'car)
(setf lat1 '(cons cdr car null eq))
(setf b 'motor)
(setf lat2 '(car engine auto motor))

(index_ a_ lat1);3;correct
(index_ a_ lat2);1;correct
(index_ b lat2);4;correct

;4.6
(defun product (vec1_ vec2_)
  (cond
   ((null vec1_) vec2_)
   ((null vec2_) vec1_)
   (t (cons (* (car vec1_)(car vec2_))
            (product 
             (cdr vec1_)(cdr vec2_))))))

(product vec1 vec2);(3 4 4);correct
(product vec2 vec3);(6 2 12);correct
(product vec3 vec4);(12 2 3);correct

;4.7
(defun dot-product (vec1_ vec2_)
  (cond
   ((null vec1_) 0)
   ((null vec2_) 0)
   (t (+ (* (car vec1_) (car vec2_))
         (dot-product
          (cdr vec1_)(cdr vec2_))))))

(dot-product vec2 vec2);29;correct
(dot-product vec2 vec4);26;correct
(dot-product vec3 vec4);17;correct

;4.8

;12 / 3 = 9 / 3 + 1
; 9 / 3 = 6 / 3 + 1
; 6 / 3 = 3 / 3 + 1
; 3 / 3 = 0 / 3 + 1

(defun /_(m n)
  (cond
   ((zero m) 0)
   ;((= 0 m) 0)
   ((< m 0) 0)
   ((< m n) 0)
   ;((zero n) 0);actually infinity
   (t (+ 1 (/_ (- m n) n)))))

(/_ 12 3);4;correct
(/_ 13 3);4;correct
(/_  7 5);1;correct
(/_  8 2);4;correct
(/_  2 3);0;correct

;4.9
(defun remainder (n m)
  (cond
   (t (- n (* m (/_ n m))))))

(remainder 12 3);0;correct
(remainder 13 3);1;correct
(remainder  7 5);2;correct
(remainder  8 2);0;correct
(remainder  2 3);2;correct

;4.10;lessThanEqualTo
(defun <=_ (m n)
  (cond
   ((eq m n) t)
   ((eq (/_ m n) 0) t)
   (t '())))

(<=_  zero one);true;correct
(<=_  one one);true;correct
(<=_  three one);false;correct





   
   












  
















