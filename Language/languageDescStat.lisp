@feride

;Язык программирования descStat реализующий основные функции описательной статистики.
;Числовые данные задаются после !, необходимые функции задаются после =.




(defun square (i)
    (expt i 2)
    )

;среднее значение
(defun mean (l)
    (+ (/ (apply '+ l)(length l)) 0.0)
    )

;медиана
(defun median (l)
    (let ( (n (length l)) )
         (cond ((= (rem n 2) 0)  "This data has no median")
             (t(nth (floor n 2) l))
             ))
    )

;размах
(defun range (l)
    (- (apply 'max l)(apply 'min l))
    )

;дисперсия
(defun disp (l)
    (mean (mapcar (lambda (x)(square (- x (mean l)))) l))
    )

;стандартное отклонение
(defun sd (l)
    (sqrt (disp l))
    )

;стандартная ошибка среднего
(defun se (l)
    (/ (sd l) (sqrt (length l)))
    )

;коэффициент вариации
(defun var (l)
    (/ (sd l) (mean l))
    )

;список чисел
(defun num (x)
    (cond ((eq (car x) '=) x)
        (t(num (cdr x))))
    )

;список функций
(defun func (x)
    (cond ((eq (car x) '=) nil)
        (t(cons (car x) (func (cdr x)))))
    )

;вычисления функций для заданных данных
(defmacro ! (&rest x)
    `(format t "~{~a~^ ~%~}~%~%~%" (mapcar (lambda (y) (funcall y (func ',x))) (remove '= (num ',x))))
    )
