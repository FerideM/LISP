;Вариант 13, задачи: 9,12,15,19,21,27,33,39,45,47
;_________________________________________
;№9
;Определите функцию, разделяющую исходный список на два подсписка. 
;В первый из них должны попасть элементы с нечетными номерами, во второй - элементы с четными номерами.
;Пример: (7 8 10 11 3 0 5) -> (( 7 10 3 5) (8 11 0)) 

(defun sep (lst)
    (cond ((null lst) nil)     
        (t(cons
           (cons (car lst) (car (sep (cddr lst))) )
           (list
            (cond ((null (cadr lst)) (cadr (sep (cddr lst)) ) )
                (t (cons (cadr lst) (cadr (sep (cddr lst))) ) )
                )
            )
           )
          )
        )
    )

(print "/////////////////////////")
(print "Task 9")
;TEST 1
(print (sep '(+ a 14 T (NIL @ _) b (a c))))
;TEST 2
(print (sep '(a b (c d) e)))
;TEST 3
(print (sep '(7 8 10 11 3 0 5)))
(print "/////////////////////////")



;__________________________________________
;№12
;Определите функцию, заменяющую в исходном списке два подряд идущих одинаковых элемента одним
;Пример: (a a b c c) -> (a b c)

(defun delete-pairs (lst)
    (dlt lst (car lst))
    )

(defun dlt (lst lst1)
    (cond((null lst) nil)
        ((equal lst1 (cadr lst)) (cons lst1 (delete-pairs (cddr lst))))
        (t(cons lst1 (delete-pairs (cdr lst))))
        )
    )

(print "/////////////////////////")
(print "Task 12")
;TEST 1
(print (delete-pairs '(a (a b) (a b) c c)))
;TEST 2
(print (delete-pairs '(a a a c c)))
;TEST 3
(print (delete-pairs '(1 3 3 5 6)))
(print "/////////////////////////")



;__________________________________________
;№15
;Определите функцию, вычисляющую скалярное произведение векторов,
;заданных списками целых чисел
;Пример: вектор (5 8 2) и вектор (3 5 7) -> 69

(defun scalar-product (list1 list2)
    (cond((null list1) 0)
        ((null list1) 0)
        (t(+ (* (car list1) (car list2)) (scalar-product (cdr list1) (cdr list2))))
        )
    )

(print "/////////////////////////")
(print "Task 15")
;TEST 1
(print (scalar-product '(2 4 2) '(3 5 4)))
;TEST 2
(print (scalar-product '(1 9) '(4 7)))
;TEST 3
(print (scalar-product '(7 4 4) '(6 5 8)))
(print "/////////////////////////")



;__________________________________________
;№19
;Определите функцию (ЛУКОВИЦА, n), строящую N-уровневый вложенный список,
;элементом которого на самом глубоком уровне является N
;Пример: (ЛУКОВИЦА, 5) -> (((((5)))))

(defun onion (n)
    (oni n n)
    )
(defun oni (n k)
    ((lambda (res)
             (cond((= n 0) 0)
                  ((= k 1) res)
                  (t(cons (oni n (- k 1)) nil))))
        (list n))
                
    )

(print "/////////////////////////")
(print "Task 19")
;TEST 1
(print (onion 7))
;TEST 2
(print (onion 2))
;TEST 3
(print (onion 13))
(print "/////////////////////////")



;__________________________________________
;№21
;Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.
;Пример: delete('a '(f h (a b) a a) -> (f h (a b) a)

(defun dlt (sample lst)
    (cond((null lst) nil)
        ((eq sample (car lst)) (cdr lst))
        (t(cons (car lst) (dlt sample (cdr lst))))
        )
    )

;(print (remove 'a '((a b) a a nil nil c c) :count 1)) ;можно без функции

(print "/////////////////////////")
(print "Task 21")
;TEST 1
(print (dlt 'a '(a (a b) c c)))
;TEST 2
(print (dlt '7 '(5 7 3 7 2 7)))
;TEST 3
(print (dlt 'c '(a 4 y c d j x c)))
(print "/////////////////////////")



;__________________________________________
;№27
;Определите функцию, которая, чередуя элементы списков (a b ...) и (1 2 ...), образует новый список (a 1 b 2 ...).
;Пример: (4 7 f 5 f) (f 7 s 3 6) -> (4 f 7 7 f s 5 3 f 6)

(defun app (lst1 lst2)
    (cond((null lst1) lst2)
        ((null lst2) lst1)
        (t(cons (car lst1)
                (cons (car lst2) (app (cdr lst1) (cdr lst2))) )
          )
        )
    )

(print "/////////////////////////")
(print "Task 27") 
;TEST 1
(print (app '(7 10 3 5) '(8 11 0)))
;TEST 2
(print (app '(A (C D) W) '(B E F)))
;TEST 3
(print (app '(a b c) '(1 2 3 4)))
(print "/////////////////////////")



;__________________________________________
;№33
;Определите функцию МНОЖЕСТВО, преобразующую список в множество.
;Пример: (4 6 2 6 2 5 7 3 2) -> (4 6 2 5 7 3)

(defun sett (lst)
    (cond((null lst) nil)
        (t (cons (car lst) (sett (remove (car lst) (cdr lst))))
          )
        ))

(print "/////////////////////////")
(print "Task 33")
;TEST 1
(print (sett '(1 2 3 2 4 5 2 1 3)))
;TEST 2
(print (sett '(1 2 3 1 2 3 1 2 3 13)))
;TEST 3
(print (sett '(f g s h o f s e d g x s g s)))
(print "/////////////////////////")



;__________________________________________
;№39
;Определите функцию СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ, формирующую множество из элементов не входящих в оба множества.
;Пример: (4 6 2 7 2 5) (4 2 7 2 7 1) -> (6 5 1)

(defun check (a st)
    (cond((null st) nil) 
        ((eq a (car st)) t) 
        (t (check a (cdr st)))
        )
    )

(defun dif (a b)
    ((lambda (l)
    (cond((null a) b)
        ((null b) a)
        ((check l b)(dif (remove l a)(remove l b)))
        (t(cons (car a) (dif (cdr a) b)))
          )
        )(car a))
    )

(print "/////////////////////////")
(print "Task 39")
;TEST 1
(print (dif '(1 2 3 1 13) '(1 5 3 2)))
;TEST 2
(print (dif '(1 3 4 5 6 7) '(3 2 4 7)))
;TEST 3
(print (dif '(17 9) '(8 6)))
;TEST 4
(print (dif '(a r f g w) '(r h s c)))
(print "/////////////////////////")



;__________________________________________
;№45
;Предположим, что у имени города есть свойства x и y, которые содержат координаты места нахождения города 
;относительно некоторого начала координат. Напишите функцию (РАССТОЯНИЕ a b),
;вычисляющую расстояние между городами a и b.

(defun square (i)
    (expt i 2)
    )
(defun diff (a b)
    (list (- (get a 'x) (get b 'x)) (- (get a 'y) (get b 'y)))
    )
(defun dist (a b)
    (
        sqrt (+ (square (car (diff a b))) (square (cadr (diff a b))))
        )
    )

(setf (get 'london 'x) 2)
(setf (get 'london 'y) 3)
(setf (get 'exeter 'x) 5)
(setf (get 'exeter 'y) 1)

(setf (get 'moscow 'x) 1)
(setf (get 'moscow 'y) -2)
(setf (get 'sochi 'x) 4)
(setf (get 'sochi 'y) -2)

(setf (get 'miami 'x) -3)
(setf (get 'miami 'y) 5)
(setf (get 'atlanta 'x) -3)
(setf (get 'atlanta 'y) 2)

(print "/////////////////////////")
(print "Task 45")
;TEST 1
(print (dist 'london 'exeter))
;TEST 2
(print (dist 'miami 'atlanta))
;TEST 3
(print (dist 'atlanta 'exeter))
(print "/////////////////////////")



;__________________________________________
;№47
;Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства символа.

(defun del (a)
    (cond((null (symbol-plist a))nil)
        (t(remprop a (car (symbol-plist a)))(del a))
        )
    )

(setf (get 'a 'x) 2)
(setf (get 'a 'y) 3)

(setf (get 'b 'x) 5)
(setf (get 'b 'y) 1)

(setf (get 'atlanta 'x) -3)
(setf (get 'atlanta 'y) 2)

(print "/////////////////////////")
(print "Task 47")
;TEST 1
(print (del 'a))
;TEST 2
(print (del 'b))
;TEST 3
(print (del 'atlanta))
(print "/////////////////////////")


