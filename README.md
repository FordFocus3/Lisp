# Lisp
# Задание 9. 
 Определите функцию, разделяющую исходный список на два подсписка. В
первый из них должны попасть элементы с нечетными номерами, во второй —
элементы с четными номерами.

Код:
``` 

(defun sub-lists(lst)
      (cond
         ((null lst) '(() ()))
         ( (evenp (car lst)) (cons (car (sub-lists(cdr lst))) (cons (cons (car lst)   (cadr (sub-lists(cdr lst))))())))
         (t(cons (cons (car lst) (car (sub-lists(cdr lst)))) (cdr (sub-lists(cdr lst)))))
       )
)
(print(sub-lists '(1 2 3 4 5 6)))
``` 

Тесты:
``` 
((1 3 5) (2 4 6)) 
``` 

# Задание 19.

Определите функцию (ЛУКОВИЦА n), строящую N-уровневый вложенный список, элементом которого на самом глубоком уровне является N.

Код:
``` 
(defun луковица (n)
    (_луковица n n))
(defun _луковица (n v)
    (cond((= n 0)v)
        (t(cons (_луковица (- n 1) v) nil))))

(princ "(луковица 5)")
(print (луковица 5))
(write-line "")
(princ "(луковица 2)")
(print (луковица 2))
``` 

Тесты:
``` 
(луковица 5)
(((((5))))) 
(луковица 2)
((2)) 
``` 

#25 

Определите функцию, удаляющую из списка каждый четный элемент

Код:
``` 
(defun filter (lst)
    (cond((null lst)nil)
        (t(cons(car lst)
            (filter (cddr lst))))))
(princ ">> (1 2 3 4)")
(print (filter '(1 2 3 4)))
(write-line "")
(princ ">> (1 2 3 4 5 6)")
(print (filter '(1 2 3 4 5 6)))
``` 

Тесты: 
``` 
>> (1 2 3 4)
(1 3) 
>> (1 2 3 4 5 6)
(1 3 5) 
``` 







