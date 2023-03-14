
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (> low high)
       null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda(s) (string-append s suffix))xs))

(define (list-nth-mod xs n)
  (cond [(< n 0)(error "list-nth-mod: n must be non-negative")]
        [(null? xs)(error "list-nth-mod: list must be non-empty")]
        [#t (let* ([i (remainder n (length xs))])
            (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (if (= 0 n)
      null
      (let ([next (s)])
        (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (n)(cons (if (= (remainder n 5) 0)(- n) n)
                               (lambda () (f (+ n 1)))))])
    (lambda() (f 1))))

(define dan-then-dog
  (letrec ([dan (lambda() (cons "dan.jpg" dog))]
           [dog (lambda() (cons "dog.jpg" dan))])
                  dan))

(define (stream-add-zero s)
  (lambda() (let([next (s)])
              (cons(cons 0 (car next))(stream-add-zero (cdr next))))))

(define (cycle-lists xs ys)
  (letrec ([loop (lambda(n)
                  (cons (cons (list-nth-mod xs n)
                              (list-nth-mod ys n))
                              (lambda() (loop (+ n 1)))))])
    (lambda()(loop 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda(x)
                (if (= x (vector-length vec))
                    #f
                    (let ([cell (vector-ref vec x)])
                    (if (and (cons? cell)(equal? v (car cell)))
                        cell
                        (f (+ x 1))))))])
    (f 0)))

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [next 0])
    (lambda(v)
      (or (vector-assoc v cache)
          (let ([ans (assoc v xs)])
            (and ans
                (begin (vector-set! cache next ans)
                       (set! next
                             (if (= (+ next 1) n)
                                 0
                                 (+ next 1)))
                       ans)))))))
(define-syntax while-less
  (syntax-rules (do)
    ((while-less x do y)
     (let ([z x])
       (letrec ([loop (lambda()
                       (let ([w y])
                         (if (or (not (number? w))(>= w z))
                             #t
                             (loop))))])
       (loop))))))