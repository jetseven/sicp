(define (cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
	  ((= pick 2) b))))

(define (car x) (x 1))

(define (cdr x) (x 2))

;; (car (cons 37 49)) -> 37
;; (cdr (cons 37 49)) -> 49

;; substitution model

(car (cons 37 49))

(car (lambda (pick)
       (cond ((= pick 1) 37)
	     ((= pick 2) 49))))

((lambda (pick)
   (cond ((= pick 1) 37)
	 ((= pick 2) 49)))
 1)

(cond ((= 1 1) 37)
      ((= 1 2) 49)))

37

;; exer 2.1
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d))
	(neg (lambda (x) (- 0 x))))
    (cond ((and (< n 0) (< d 0)) (cons (/ (neg n) g) (/ (neg d) g)))
	  (else (cons (/ n g) (/ d g))))))

(define (make-rat n d)
  (let ((g (gcd n d))
	(neg (lambda (x) (- 0 x))))
    (cond ((and (< n 0) (> d 0)) (cons (neg (/ n g)) (neg (/ d g))))
	  (else (cons (/ n g) (/ d g))))))

(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (< d 0)
	(cons (- (/ n g)) (- (/ d g)))
	(cons (/ n g) (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;Exercise 2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))
(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
	      (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

(define p1 (make-point 0 0))
(define p2 (make-point 3 3))
(define s1 (make-segment p1 p2))
(print-point (midpoint-segment s1))
		   
;; exercise 2.3

(define (make-rect bottom-left top-right)
  (cons bottom-left top-right));;;SECTION 2.1.4

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; EXERCISE 2.7

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

;; EXERCISE 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

;; EXERCISE 2.10

(define (div-interval x y)
  (if (and (> 0 (lower-bound y)) (< 0 (upper-bound y)))
      (error "Divide by zero")
        (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

;; EXERCISE 2.11
(define (mul-interval x y)
  (define (has-neg z)
    (or (> 0 (upper-bound z)) (> 0 (lower-bound z))))
  (if (has-neg x)