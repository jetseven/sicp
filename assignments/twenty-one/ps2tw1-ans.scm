(define (stop-at n)
  (lambda (your-hand opponent-up-card)
    (if (>= (hand-total your-hand) n)
    #f
    #t)))

(define (test-strategy player-strategy house-strategy reps)
  (define (iter count result)
    (if (= count 0)
	result
	(iter (- count 1) (+ result (twenty-one player-strategy house-strategy)))))
  (iter reps 0))

(define (watch-player strategy)
  (lambda (your-hand opponent-up-card)
    (display "Your hand: ")
    (display (hand-total your-hand))
    (display " ")
    (display "Opponent's Up card: ")
    (display opponent-up-card)
    (newline)
    (if (strategy your-hand opponent-up-card)
	(display "Hit")
	(display "Stand"))
    (newline)
    (display "----------")
    (newline)
    (strategy your-hand opponent-up-card)))

(define (louis your-hand opponent-up-card)
  (cond ((< (hand-total your-hand) 12) #t)
	((> (hand-total your-hand) 16) #f)
	((= (hand-total your-hand) 12)
	 (if (< opponent-up-card 4)
	     #t
	     #f))
	((= (hand-total your-hand) 16)
	 (if (= opponent-up-card 10)
	     #f
	     #t))
	(else (if (> opponent-up-card 6)
		  #t
		  #f))))