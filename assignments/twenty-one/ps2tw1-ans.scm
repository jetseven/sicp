(define (stop-at n)
  (lambda (my-hand opponent-up-card)
    (if (>= (hand-total my-hand) n)
    #f
    #t)))

(define (test-strategy player-strategy house-strategy reps)
  (define (iter count result)
    (if (= count 0)
	result
	(iter (- count 1) (+ result (twenty-one player-strategy house-strategy)))))
  (iter reps 0))

(define (watch-player strategy)
  (lambda (my-hand opponent-up-card)
    (display "Your hand: ")
    (display (hand-total my-hand))
    (display " ")
    (display "Opponent's Up card: ")
    (display opponent-up-card)
    (newline)
    (if (strategy my-hand opponent-up-card)
	(display "Hit")
	(display "Stand"))
    (newline)
    (display "----------")
    (newline)
    (strategy my-hand opponent-up-card)))

(define (louis my-hand opponent-up-card)
  (cond ((< (hand-total my-hand) 12) #t)
	((> (hand-total my-hand) 16) #f)
	((= (hand-total my-hand) 12)
	 (if (< opponent-up-card 4)
	     #t
	     #f))
	((= (hand-total my-hand) 16)
	 (if (= opponent-up-card 10)
	     #f
	     #t))
	(else (if (> opponent-up-card 6)
		  #t
		  #f))))

;; Problem 6
(define (both first-strategy second-strategy)
  (lambda (my-hand opponent-up-card)
    (and (first-strategy my-hand opponent-up-card)
	 (second-strategy my-hand opponent-up-card))))

