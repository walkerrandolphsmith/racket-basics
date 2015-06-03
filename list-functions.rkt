;List functions

(define make-seq-list
  (lambda (x y)
    (cond
      ((< y x) '())
      (else  (cons x (make-seq-list (+ x 1) y))))))

(define make-whole-list
  (lambda (x)
    (gen x 1)))

(define gen
  (lambda (x y)
    (cond
      ((> y x) '())
      (else  (cons y (gen x (+ y 1)))))))

(define find-ith-element
  (lambda (x y)
    (cond
      ((= y 0) (car x))
      (else (find-ith-element (cdr x) (- y 1))))))

(define last-element
  (lambda (x)
    (find-last x (length x))))
(define find-last
  (lambda (x y)
    (cond
      ((= y 1)(car x))
      (else (find-last (cdr x) (- y 1))))))

(define list-length
  (lambda (x)
    (length x)))

(define concat-list
  (lambda (x y)
    (cond
      ((null? x)y)
      ((null? y)x)
      ((>(length x)0)(cons (car x)(concat-list (cdr x) y)))
      (else (cons (car y)(concat-list x (cdr y)))))))
(define merge-sort
  (lambda (x y)
    (cond
      ((null? x)y)
      ((null? y)x)
      ((<(car x)(car y))(cons (car x)(concat-list (cdr x) y)))
      (else (cons (car y)(concat-list x (cdr y)))))))

(define reverse-list
  (lambda (x)
    (reverse x)))

(define max
  (lambda (x)
    (let ((head (car x)) (tail (cdr x)))
      (if (null? tail)head
        (let ((max-in-tail (max tail)))
          (if (> head max-in-tail) head max-in-tail))))))

(define min
  (lambda (x)
    (let ((head (car x)) (tail (cdr x)))
      (if (null? tail)head
        (let ((max-in-tail (min tail)))
          (if (< head max-in-tail) head max-in-tail))))))

(define even
  (lambda (x)
    (get-e x 0)))
(define get-e
  (lambda (x  y)
    (cond
      ((null? x) '())
      ((even? y)(cons (car x)(get-e (cdr x) (+ y 1))))
      ((odd? y)(get-e (cdr x) (+ y 1)))
      )))

(define odd
  (lambda (x)
    (get-o ( cdr x) 1)))
(define get-o
  (lambda (x  y)
    (cond
      ((null? x) '())
      ((odd? y)(cons (car x)(get-o (cdr x) (+ y 1))))
      ((even? y)(get-o (cdr x) (+ y 1)))
      )))
