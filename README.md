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
