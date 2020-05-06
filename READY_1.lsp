; #9 Определите функцию, разделяющую исходный список на два подсписка. В
; первый из них должны попасть элементы с нечетными номерами, во второй —
; элементы с четными номерами.

(defun split (lst)
    (cond((null lst)'(nil nil))
        (t((lambda (l f s)                        
                (cons(cons f (car l))(cons                           
                        (cond((null s)(cadr l))
                            (t(cons s (cadr l))))nil)))
              (split (cddr lst)) (car lst) (cadr lst)))))

; Test 1
(print (split '(1 2 3 4)))
; Test 2
(print (split '(1 2 3)))




;
;------------------------------------------------------------------------------
;



; #29 Определите функцию, вычисляющую глубину списка (самой глубокой ветви).

(defun depth (lst)
    (cond((null lst)0)
        (t(max(+ (depth (car lst)) 1)(depth (cdr lst))))))

; Test 1
(print (depth '(nil (nil) nil)))
; Test 2
(print (depth '(nil (nil) ((nil)))))




;
;------------------------------------------------------------------------------
;

; #34 Определите предикат РАВЕНСТВО-МНОЖЕСТВ, проверяющий совпадение двух множеств (независимо от порядка следования элементов). 

(defun equal-set (lst-a lst-b)
    (cond((and (null lst-a) (null lst-b))T)
        ((or (null lst-a) (null lst-b))nil)
        (t(equal-set (cdr lst-a) (remove-value lst-b (car lst-a))))))
(defun remove-value (lst v)
    (cond((null lst)nil)
        (t((lambda (e)(cond((= (car lst) v)e)
                    (t(cons (car lst) e)))) (remove-value (cdr lst) v)))))

; Test 1
(print (equal-set '(1 2 3) '(3 2 1)))
; Test 2
(print (equal-set '(1 2 3) '(4 5 6)))
; Test 3
(print (equal-set '(1 2) '(1 2 3)))
; Test 4
(print (equal-set '() '()))




;
;------------------------------------------------------------------------------
;

; #36 Определите предикат НЕПЕРЕСЕКАЮЩИЕСЯ, проверяющий, что два множества не
; пересекаются, т.е. у них нет общих элементов.

(defun intersect (lst-a lst-b)(cond
        ((null lst-a)nil)
        ((list-contains lst-b (car lst-a))t)
        (t(intersect (cdr lst-a) lst-b))))
(defun list-contains (lst v)
    (cond((null lst)nil)
        ((= (car lst) v)t)
        (t(list-contains (cdr lst) v))))
; Test 1
(print (intersect '(1 2 3) '(3 4 5)))
; Test 2
(print (intersect '(1 2 3) '(4 5 6)))




;
;------------------------------------------------------------------------------
;


; #40 Определите функцию РАЗНОСТЬ, формирующую разность двух множеств, т.е.
; удаляющую из первого множества все общие со вторым множеством элементы.


(defun subtract (lst-a lst-b)
    (cond((null lst-b)lst-a)
        (t(subtract (remove-value lst-a (car lst-b)) (cdr lst-b)))))

; Test 1
(print (subtract '(1 2 3) '(2 3 4)))


; Test 2
(print (subtract '(1 2 3) '(4 5 6)))




;
;------------------------------------------------------------------------------
;


; #45 Предположим, что у имени города есть свойства х и у, которые содержат координаты
; места нахождения города относительнонекоторого начала координат.


(defun distance (city-a city-b)
    (sqrt (+(expt(- (get city-a 'x) (get city-b 'x))2)
        (expt(- (get city-a 'y) (get city-b 'y))2))))
(defun make-city (name x y)
        (setf (get name 'x) x)
        (setf (get name 'y) y))

; Test 1
(make-city 'city1 0 3)
(make-city 'city2 4 0)
(print (distance 'city1 'city2))


; Test 2
(make-city 'city1 34 56)
(make-city 'city2 71 66)
(print (distance 'city1 'city2))



;
;------------------------------------------------------------------------------
;


; #46 Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих свойств у символа, 
; обозначающего это лицо. Напишите функцию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей,
; и предикат (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, если x1 и x2 — сестры или братья, родные или с одним общим родителем



(defun make-child (name p1 p2)
    (setf (get name 'p1) p1)
    (setf (get name 'p2) p2))
(defun parents (x)
    (cons(get x 'p1)
        (cons(get x 'p2)nil)))
(defun relatives (x1 x2)
    ((lambda (e1 e2)
        (or
            (string= (car e1) (car e2))
            (string= (cadr e1) (car e2))
            (string= (car e1) (cadr e2))
            (string= (cadr e1) (cadr e2)))) 
            (parents x1) (parents x2)))

(make-child 'Vika 'A 'B)
(make-child 'Dasha 'B 'C)
(make-child 'Ira 'C 'D)
; Test 1
(print (relatives 'Vika 'Dasha))
; Test 2
(print (relatives 'Vika 'Ira))

