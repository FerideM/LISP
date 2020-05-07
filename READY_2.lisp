;Вариант 13, задачи: 1,3,5,7,9,11,13

;__________________________________________
;№9
;Напишите генератор порождения чисел Фибоначчи: 0,1,1,2,3,5,...

(defun fib ()
    (let ((x 0) 
          (y 1))
         (lambda ()
             (car (list x 
                        (setq z x) 
                        (setq x y)
                        (setq y (+ z y)))))))

(setq fb (fib))

(print "/////////////////////////")
(print "Task 9")
;TEST 

(print (funcall fb))
(print (funcall fb))
(print (funcall fb))
(print (funcall fb))
(print (funcall fb))
(print (funcall fb))
(print (funcall fb))
(print (funcall fb))

(print "/////////////////////////")



;__________________________________________
;№11
;Опредеите функционал МНОГОФУН, который использует функции,являющиеся аргументами, по следующей схеме
;(МНОГОФУН '(f g ... h) x) <=> (LIST (f x) (g x) ... (h x)).

(defun mnogofun (f x)
    (mapcar (lambda (a) (apply a x)) f)
        )


(print "/////////////////////////")
(print "Task 11")
;TEST 1
(print (mnogofun '(* min) '(5 -8 9)) )
;TEST 2
(print (mnogofun '(expt log) '(2 5)) )
;TEST 3
(print (mnogofun '(+ - max) '(6 3 7 9 53)) )
(print "/////////////////////////")



;__________________________________________
;№13
;Определите функцию, которая возвращает в качестве значения свое определение (лямбда-выражение).

(setq smt 
      '((lambda (x) (list x (list 'quote x)))
        '(lambda (x) (list x (list 'quote x)))
        )
      )


(print "/////////////////////////")
(print "Task 13")
;TEST 1
(print (eval smt))
;TEST 2
(print (eval (eval smt)))
;TEST 3
(print (eval (eval (eval smt))))
;TEST 4
(print (eval (eval (eval (eval smt)))))
(print "/////////////////////////")


;/////////////////////////
;/////////////////////////
;/////////////////////////
;Засчитанные
;/////////////////////////
;/////////////////////////
;_________________________________________
;№1
;Определить FUNCALL через функционал APPLY.

(defun funcal (fn &rest a)
    (apply fn a)
    )


(print "/////////////////////////")
(print "Task 1")
;TEST 1
(print (funcal '+ 2 3 7 4 5 1))
;TEST 2
(print (funcal '/ 10 5))
;TEST 3
(print (funcal #'(lambda (x y) (* x (* y 2))) 9 3))
(print "/////////////////////////")



;__________________________________________
;№3
;Опрделите функционал (APL-APPLY f x), который применяет функцию fi списка
;(f1 f2 ... fn)
;к соответствующему элементу списка
;x=(x1 x2 ... xn)
;и возвращает список, сформированный из результатов.

(defun apl-apply (f x)
    (mapcar 'apply f x)
    )


(print "/////////////////////////")
(print "Task 3")
;TEST 1
(print (apl-apply '(+ - min) '((4 3) (6 9) (5 7))) )
;TEST 2
(print (apl-apply '(* max) '((12 5) (5 4 -6 55 7))) )
;TEST 3
(print (apl-apply '(/ / /) '((10 5) (12 3 4) (18 3 2))) )
(print "/////////////////////////")



;__________________________________________
;№7
;Определите фильтр  (УДАЛИТЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список все элементы,
;которые не обладают свойством, наличие которого проверяет предикат пред.

(defun dlt-if-not (pred lst)
	  (mapcan (lambda (a) (if (funcall pred a) (list a))) lst)
)


(print "/////////////////////////")
(print "Task 7")
;TEST 1
(print (dlt-if-not (lambda (a)(< a 4)) '(0 1 2 3 4 5)))
;TEST 2
(print (dlt-if-not (lambda (a) (= a 0)) '(1 3 5 7)))
;TEST 3
(print (dlt-if-not (lambda (a) (> a 0)) '(1 2 -6 3 4)))
(print "/////////////////////////")




