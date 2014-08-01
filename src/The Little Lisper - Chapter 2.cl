;The Little Lisper - Chapter 2

(setf l1 '(german chocolate cake))
(setf l2 '(poppy seed cake))
(setf l3 '((linzer) (torte)()))
(setf l4 '((bleu cheese)(and)(red)(wine)))
(setf l5 '(()()))
(setf a1 'coffee)
(setf a2 'seed)
(setf a3 'poppy)

(defun lat? (l)
  (cond
   ((null l) t)
   ((atom (car l)) (lat? (cdr l)))
   (t nil)))

;1.1
(lat? l1)
;true (T)
(lat? l2)
;true (T)
(lat? l3)
;false (NIL)


;2.2
;Recommend you hilight parts of the function and evaluate them

;2.3
;(member? a1 l1)
;false (NIL)

(member a2 l2)
;true (seed cake)

;2.4 

;(define member?
;    (lambda (a lat)
;  (cond
;   ((null lat) NIL)
;   (t (or
;       (eq (car lat) a)
;       (member? a (cdr lat)))))))

(defun member? (a lat)
  (cond
   ((null lat) NIL)
   (t (or
       (eq (car lat) a)
       (member? a (cdr lat))))))

(defun member?  (a lat)
      (if (null lat)
          NIL
        (or
         (eq (car lat) a)
         (member? a (cdr lat)))))

;(define lat? 
;    (lambda(l)
;  (cond
;   ((null l)t)
;   ((atom (car l))(lat cdr( l)))
;   (t NIL))))

(defun lat? (l)
  (cond
   ((null l)t)
   ((atom (car l))(lat cdr( l)))
   (t nil)))

;(defun lat? (l)
;  (if (null l)
;      t
;    (if ((atom (car l)))
;        (lat (cdr l)))
;    nil))

;2.5
(defun nonlat? (l)
  (cond
   ((null l) nil)
  ((atom (car l))(nonlat? (cdr l)))
   (t t)))

(defun nonlat? (l)
  (cond
   ((null l) T)
  ((lat? l) nil)
   (t t)))

;(lat? l1)

;(nonlat? l1)
;false (NIL) - correct
;(nonlat? l2)
;false (NIL) - correct
;(nonlat? l3)
;true - (T) - nope (actually correct - the manual is wrong according to this)
;http://newsgroups.derkeiler.com/Archive/Comp/comp.lang.scheme/2008-09/msg00252.html
;http://www.cs.rutgers.edu/~ccshan/cs314/

;(nonlat? l4)
;true (T) - correct

;2.6
(defun member-cake? (lat)
  (cond 
   ((null lat) nil)
   (t (or
       (eq (car lat) 'cake)
       (member-cake? (cdr lat))))))

(member-cake? l1)
;true (T) - correct
(member-cake? l2)
;true (T) - correct
(member-cake? l3)
;false (NIL) - correct

;2.7
(defun member2? (a lat)
  (cond
  ((null lat) nil)
  (t (or
      (member2? a (cdr lat))
      (eq a (car lat))))))

;They are the same - member2? gives non-lazy (inefficient)
;evaluation of the list

(member? a1 l1)
(member2? a1 l1)
; same (NIL)

(member? a1 l2)
(member2? a1 l2)
; same (NIL)

(member? a2 l2)
(member2? a2 l2)
; same (T)


;2.8 
; Ditto

;2.9
(member? a2 l3)

;(lat? ())

(defun member? (a lat)
  (cond
   ((null lat) NIL)
   (t (or
       (eq (car lat) a)
       (member? a (cdr lat))))))

;*** Not convinced an issue exists - incorrect question perhaps ***
;*** could be difference between eq and equal and eqan and =? ***

;2.10

(defun member-twice? (a lat)
  (cond
   ((member? a lat)(member? a (cdr lat)))
   (t nil)))


(member-twice? 'poppy (cons a3 l2)) ;should be true
(member-twice? 'poppy  l2) ;should be false






















