#lang racket/base

#|-----powerfour function definition-----|#
(define (powerfour n)
  (cond 
    [(<= n 0) #f]
    [(= n 1) #t]
    [(= (modulo n 4) 0) (powerfour (/ n 4))]
    [else #f]
  )  
)

#|-----insert-at function definition-----|#
(define (insert-at lst elem pos)
  (cond
    [(null? lst) (if (equal? pos 0) (cons elem lst) lst)]
    [(equal? pos 0) (cons elem lst)]
    [else (cons (car lst) (insert-at (cdr lst) elem (- pos 1)))]
  )
)

#|-----palindromep function definition-----|#
(define (palindromep lst)
  (define (reverselist lst)
    (cond
      [(null? lst)'()]
      [else (append (reverselist (cdr lst)) (list (car lst)))]
    )
  )
  (cond
    [(equal? lst (reverselist lst)) 1]
    [else 0]
  )
)
  
#|-----ifPrime function definition-----|#
(define (ifPrime n)
  (define (try-divisors d)
    (cond 
      [(> d (floor (sqrt n))) 0]
      [(= (modulo n d) 0) 1]
      [else (try-divisors (+ d 1))]
    )
  )
  (cond 
    [(<= n 1) 1]
    [(= n 2) 0]
    [else (try-divisors 2)]
  )
)


#|-----Testing powerfour function-----|#
 (powerfour 16) 
 #|Output True|#
 (powerfour 13) 
 #|Output False|#

#|-----Testing insert-at function-----|#
 (insert-at '(1 2 3) 4 2) 
 #|Output (1 2 4 3)|# 
 (insert-at '(14 53 64 22) 19 1) 
 #|Output '(14 19 53 64 22)|# 

#|-----Testing palindromep function-----|#
 (palindromep '(1 2 3 2 1))
 #|Output 1 for Palindrome|#
 (palindromep '(1 2 3 4 5))
 #|Output 0 for not Palindrome|#

#|-----Testing ifPrime function-----|#
 (ifPrime 11)
 #|Output 0 for Prime|#
 (ifPrime 32)
 #|Output 1 for not Prime|# 