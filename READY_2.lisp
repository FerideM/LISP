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


