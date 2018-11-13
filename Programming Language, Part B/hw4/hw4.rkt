#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))
;2
(define (string-append-map xs suffix)
  (map (lambda (x)
         (string-append x suffix)) xs))
;3
(define (list-nth-mod xs n)
  (cond [(< n 0 ) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car(list-tail xs (remainder n (length xs))))]))
;4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))
;5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- x) x)
                              (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
      
;6
(define dan-then-dog
  (letrec ([f (lambda (b)
                (if b
                    (cons "dan.jpg" (lambda () (f #f)))
                    (cons "dog.jpg" (lambda () (f #t)))))])
    (lambda () (f #t))))
;7
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))
;8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (let ([x (list-nth-mod xs n)]
                      [y (list-nth-mod ys n)])
                  (cons (cons x y) (lambda () (f (+ n 1))))))])
    (lambda () (f 0))))
;9
(define (vector-assoc v vec)
  (letrec ([f (lambda(n)
                (if(>= n (vector-length vec))
                       #f
                       (let ([ipr (vector-ref vec n)])
                         (if (and (pair? ipr) (equal? (car ipr) v))
                             ipr
                             (f (+ n 1))))))])
    (f 0)))
;10 
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [pos 0])
    (lambda (v)
      (or (vector-assoc v memo)
          (let ([new-ans (assoc v xs)])
            (and new-ans
                 (begin
                   (vector-set! memo pos new-ans)
                   (set! pos (remainder (+ pos 1) n))
                   new-ans)))))))
;11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([val e1]
              [loop (lambda ()
                      (if (>= e2 val)
                          #t
                          (loop)))])
       (loop))]))