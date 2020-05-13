;Вариант 13, задачи: 1,2,3,4,5,6

;__________________________________________
;№1
;Определите макрос, который возвращает свой вызов


(defmacro func (&rest a)
	`(quote (func ,@a))
)


(print "/////////////////////////")
(print "Task 1")
;TEST1 
(print(func x))
;TEST2 
(print(func (1 2) 5 *))
;TEST3 
(print(func "Hello"))
(print "/////////////////////////")



;__________________________________________
;№2
;Определите макрос (POP стек), который читает из стека верхний элемент и меняет значение переменной стека

(defmacro popp (stek)
    `(car (list 
        (car ,stek) 
        (setq ,stek (cdr ,stek)))
    )
)   

(setq stek '(1 2 3 4))

(print "/////////////////////////")
(print "Task 2")
;TEST 
(print (popp stek))
(print (popp stek))
(print (popp stek))
(print (popp stek))
(print (popp stek))
(print "/////////////////////////")



;__________________________________________
;№3
;Определите лисповскую форму (IF условие p q) в виде макроса

(defmacro iff (a p q)
    `(cond (,a ,p)
	(t ,q))
)   


(print "/////////////////////////")
(print "Task 3")
;TEST 1
(print (iff (= 0 1) 'yes 'no))
;TEST 2
(print (iff (eq 'x 'x) 'yes 'no))
;TEST 3
(print (iff (> 7 9) 'yes 'no))
(print "/////////////////////////")



;_________________________________________
;№4
;Определите в виде макроса форму (FIF тест отр нуль полож) 

(defmacro fif (test neg zero pos)
    `(cond ((= ,test 0) ,zero)
         ((> ,test 0) ,pos)
         (t ,neg))
)   


(print "/////////////////////////")
(print "Task 4")
;TEST 1
(print (fif 11 'neg 'zero 'pos))
;TEST 2
(print (fif -11 'neg 'zero 'pos))
;TEST 3
(print (fif 0 'neg 'zero 'pos))
(print "/////////////////////////")



