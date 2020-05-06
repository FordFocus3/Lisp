1 3 7 11


;;; #1
;;; Определить funcall через apply

(defun funcall2 (&rest args)
        (apply (car args) (cdr args)))

;;; Test #1
(print (funcall2 '+ 1 2 3))


;;; Test #2
(print (funcall2 'max 1 2 3))




;
;------------------------------------------------------------------------------
;



;;; #3 Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка
; (f1 f2 ... fn)
; к соответствующему элементу списка
; x = (x1 x2 ... xn)
;и возвращает список, сформированный из результатов.
;   (mapcar (lambda (f a) (apply f a)) funcs args))


(defun apl-apply (funcs args)
    (mapcar (lambda (f a) (apply f a)) funcs args))

;;; Test #1
(print (apl-apply '(min max) '((1 2 3) (1 2 3))))


;;; Test #2
(print (apl-apply '(min max) '((1 2 3) (1 2 3) nil)))


;;; Test #3
(print (apl-apply '(+ - *) '((1 2 3) (3 2 1) (2 4 6))))



;
;------------------------------------------------------------------------------
;


;;; #7
;;; Определите фильтр (УДАЛйЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список
;;; все элементы, которые не обладают свойством, наличие которого проверяет
;;; предикат пред.

(defun where (pred list)
    (mapcan (lambda (x) (cond ((funcall pred x) (list x)) (t nil))) list)
)

;;; Test #1
(print (where (lambda (x) (> x 3)) '(1 3 5 7)))


;;; Test #2
(print (where (lambda (x) (= (mod x 2) 0)) '(1 2 3 4)))

;
;------------------------------------------------------------------------------
;



; #11
;  Определите фукнционал МНОГОФУН, который использует функции, являющиеся
; аргументами, по следующей схеме:
; (МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).


(defun multifun (funcs lst)
    (mapcar (lambda (f) (apply f lst)) funcs))

;;; Test #1
(print (multifun '(+ - *) '(4 3 2 1)))

; Test #2
(print (multifun '(max min) '(4 3 2 1)))


