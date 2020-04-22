;Вариант 13, задачи: 9,12,15,19,21,27,33,39,45,47
;_________________________________________
;№9
;Определите функцию, разделяющую исходный список на два подсписка. 
;В первый из них должны попасть элементы с нечетными номерами, во второй - элементы с четными номерами.
;Пример (индексы в списке с 0): (7 8 10 11 3 0 5) -> ((8 11 0) ( 7 10 3 5)) 
(defun app (list1 list2)
    (cond ((null list1) list2)
        ((null list2) list1)
        (t (cons (car list1) (app (cdr list1) list2)))))

(defun sep (list)
    (let ((result) (odd) (even))
        (setq n (length list))
        ;(setq k 0)
        (do ((k 0)) 
            ((= n 0) result)
            (dolist (value list result)
                (if (> (rem k 2) 0)
                    (setq odd (app odd (list value)))
                    (setq even (app even (list value))))
                (setq result (list odd even))
                (setq n (- n 1))
                (setq k (+ k 1))))))

;TEST 1
(print (sep '(+ a 14 T (NIL @ _) b (a c))))
;TEST 2
(print (sep '(a b (c d) e)))
;TEST 3
(print (sep '(7 8 10 11 3 0 5)))

;__________________________________________
;№15
;Определите функцию, вычисляющую скалярное произведение векторов,
;заданных списками целых чисел
;Пример: вектор (5 8 2) и вектор (3 5 7) -> 69

(defun scalar-product (list1 list2)
    (setq n (- (length list1) 1))
    (do ((res 0))
        ((= n -1) res)
            (setq res (+ res (* (nth n list1) (nth n list2))))
        (setq n (- n 1))))

;TEST 1
(print (scalar-product '(2 4 2) '(3 5 4)))
;TEST 2
(print (scalar-product '(1 9) '(4 7)))
;TEST 3
(print (scalar-product '(7 4 4) '(6 5 8)))

;__________________________________________
;№19
;Определите функцию (ЛУКОВИЦА, n), строящую N-уровневый вложенный список,
;элементом которого на самом глубоком уровне является N
;Пример: (ЛУКОВИЦА, 5) -> (((((5)))))

(defun onion (n)
    (do ((res (list n)))
        ((= n 1) res)
            (setq res (list res))
        (setq n (- n 1))))

;TEST 1
(print (onion 7))
;TEST 2
(print (onion 2))
;TEST 3
(print (onion 13))
