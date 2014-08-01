;The Little Lisper
;Chapter 3

(setf l1 '((paella spanish)(wine red)(and beans)))
(setf l2 '())
(setf l3 '(cincinnati chili))
(setf l4 '(texas hot chili))
(setf l5 '(soy sauce and tomato sauce))
(setf l6 '((spanish)()(paella)))
(setf l7 '((and hot)(but dogs)))
(setf a1 'chili)
(setf a2 'hot)
(setf a3 'spicy)
(setf a4 'sauce)
(setf a5 'soy)

;3.1
(defun firsts (l)
  (cond
   ;((null l) '());incorrect
   ((null l) NIL);correct
   (t (cons (car (car l)) 
                 (firsts (cdr l))))))

(firsts l1)

(defun seconds (l)
  (cond
   ((null l) nil)
   ;((null l) '());incorrect
   (t (cons (car (cdr (car l))) 
;   (t (cons  (cdr (car l));incorrect
            (seconds (cdr l))))))

(seconds l1)

;3.2
(defun dupla (a l)
  (cond
   ((null l) '())
   (t (cons a (dupla a (cdr l))))))

(dupla a2 l4)
(dupla a2 l2)
(dupla a1 l5)

;3.3
(defun rember (a lat)
  (cond 
   ((null lat) '())
   (t (cond
       ((eq (car lat) a) (cdr lat))
       (t (cons (car lat)
                (rember
                 a (cdr lat))))))))

(rember 'and '(bacon lettuce and tomato))

(defun double (a lat)
  (cond
   ((null lat) '())
   ((eq (car lat) a)(cons a lat))
   (t (cons (car lat)
            (double a (cdr lat))))))


(double a2 l2)
(double a1 l3)
(double a2 l4)

;3.4
(defun subst-sauce (a lat)
  (cond
   ((null lat) '())
   (t (cond
       ((eq (car lat) 'sauce)
        (cons a (cdr lat)))
       (t (cons (car lat)
                    (subst-sauce a (cdr lat))))))))
;l4;(texas hot chili)
;a1;chili
(subst-sauce a1 l4);correct (texas hot chili)
(subst-sauce a1 l5);correct (soy chili and tomato sauce)
(subst-sauce a4 l2);correct NIL

;3.5
(defun subst3 (new o1 o2 o3 lat)
  (cond 
   ((null lat) '())
   (t (cond
       ((eq (car lat) o1)
        (cons new (cdr lat)))
       ((eq (car lat) o2)
        (cons new (cdr lat)))
       ((eq (car lat) o3)
        (cons new (cdr lat)))
       (t (cons (car lat)
                (subst3 new o1 o2 o3 (cdr lat))))))))

(subst3 a5 a1 a2 a4 l5); (soy soy and tomato sauce) ;correct
(subst3 a4 a1 a2 a3 l4); (texas sauce chili); correct
(subst3 a3 a1 a2 a5 l2); () ; correct

;3.6
(defun substN (new slat lat)
  (cond
   ((null lat) '())
   (t (cond
       ((member (car lat) slat)
        (cons new (cdr lat)))
       (t (cons (car lat)
                (substN new slat (cdr lat))))))))

(substN a2 l3 l4);correct
(substN a4 l3 l5);correct
(substN a4 l3 l2);correct

;3.7 step through manually

;3.8
;Typical elements - possible results within the list result **?**
;Natural recursion - what you cons onto
;3.1 Natural recursion - (firsts (cdr l)) - (seconds (cdr l))
;3.2 Natural recursion - (dupla a (cdr c))
;3.3 Natural recursion - (rember a (cdr lat))
;3.4 Natural recursion - (subst-sauce a (cdr lat))
;3.5 Natural recursion - (subst2 new o1 o2 o3 (cdr lat))
;3.6 Natural recursion - (substN new slat (cdr lat))


;3.9
(defun rember (a lat)
  (cond 
   ((null lat) '())
   (t (cond
       ((eq (car lat) a) (cdr lat))
       (t (cons (car lat)
                (rember
                 a (cdr lat))))))))


(defun rember (a lat)
  (cond
   ((null lat) '())
   ((eq (car lat) a)(cdr lat))
   (t (cons (car lat)
            (rember a (cdr lat))))))

(rember a1 l3);(cincinnati);correct
(rember a4 l5);(soy and tomato sauce);correct
(rember a4 l2);NIL;correct
  
(defun rember2 (a lat)
  (cond
   ((null lat) '())
   ((eq (car lat) a) (cons a (rember a (cdr lat))))
   (t (cons (car lat)
            (rember2 a (cdr lat))))))

(rember2 a1 l3);(cincinnati chili);correct
(rember2 a4 l5);(soy sauce and tomato sauce);incorrect
(rember2 a4 l2);NIL;correct

;3.10
(defun rember (a lat)
  (cond
   ((null lat) '())
   ((eq (car lat) a)(cdr lat))
   (t (cons (car lat)
            (rember a (cdr lat))))))

(defun insertR (new old lat);COMMON 1
  (cond;COMMON 1
   ((null lat) '());COMMON 1
   (t (cond;COMMON 1
       ((eq (car lat) old);COMMON 1
        (cons old
              (cons new (cdr lat))))
       (t (cons (car lat);COMMON 2
                (insertR;COMMON 2
                 new old (cdr lat))))))));COMMON 2

(defun insertL (new old lat);COMMON 1
  (cond;COMMON 1
   ((null lat) '());COMMON 1
   (t (cond;COMMON 1
       ((eq (car lat) old);COMMON 1
        (cons new
              (cons old (cdr lat))))
       (t (cons (car lat);COMMON 2
                (insertL;COMMON 2
                 new old (cdr lat))))))));COMMON 2

(defun subst_ (new old lat);COMMON 1
  (cond;COMMON 1
   ((null lat) '());COMMON 1
   (t (cond;COMMON 1
       ((eq (car lat) old);COMMON 1
        (cons new (cdr lat)))
       (t (cons (car lat);COMMON 2
                (subst_;COMMON 2
                 new old (cdr lat))))))));COMMON 2




                    











