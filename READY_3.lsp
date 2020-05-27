;;; #1
;;; Определите макрос, который возвращает свой вызов
(defmacro get-call nil
	`(quote (get-call))
)

;;; Test #1
(princ "  Test for task #1")
(print (macroexpand '(get-call)))
(print (get-call))
(write-line "")

;;; #2
;;; Определите макрос (POP стек), который читает из стека верхний
;;; элемент и меняет значение переменной стека

(defmacro popp (stack)
    `(let
        ((,'temp 0))
        (cond
            ((psetq ,'temp (car ,stack) ,stack (cdr ,stack)))
            (t temp)
        )
    )
)

;;; Test #2
(princ "  Test for task #2")
(setq stack1 (list 1 2 3 4))
;(print (macroexpand '(popp stack1)))
(print stack1)
(print (popp stack1))
(print stack1)
(print (popp stack1))
(print stack1)
(write-line "")

;;; #3
;;; Определите лисповскую форму (IF условие p q) в виде макроса

(defmacro iff (q p n)
    `(cond
        (,q ,p)
        (,t ,n)
    )
)

;;; Test #3
(princ "  Test for task #3")
(print (iff (> 3 2) 1 2))
(print (iff (< 3 2) 1 2))
(write-line "")
