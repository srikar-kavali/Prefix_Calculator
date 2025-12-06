#lang racket
(require "mode.rkt")

;;Mode detection from mode.rkt
(define prompt?
   (let [(args (current-command-line-arguments))]
     (cond
       [(= (vector-length args) 0) #t]
       [(string=? (vector-ref args 0) "-b") #f]
       [(string=? (vector-ref args 0) "--batch") #f]
       [else #t])))

;; Tokenizer
(define (whitespace? ch)
    (or (char-whitespace? ch) (char=? ch #\tab)))

(define (string->tokens s)
  (define len (string-length s))
  (define (loop i acc)
    (cond
      [(>= i len) (reverse acc)]
      [else
       (define ch (string-ref s i))
       (cond
         [(whitespace? ch) (loop (add1 i) acc)]
         [(char=? ch #\+) (loop (add1 i) (cons 'plus acc))]
         [(char=? ch #\*) (loop (add1 i) (cons 'mult acc))]
         [(char=? ch #\/) (loop (add1 i) (cons 'div acc))]
         [(char=? ch #\-) (loop (add1 i) (cons 'neg acc))]
         [(char=? ch #\$)
          (define-values (digits j)
            (let loop2 ((j (add1 i)) (acc ""))
              (if (and (< j len) (char-numeric? (string-ref s j)))
                  (loop2 (add1 j) (string-append acc (string (string-ref s j))))
                  (values acc j))))
          (if (string=? digits "")
              #f
              (loop j (cons (cons 'hist (string->number digits)) acc)))]
         [(char-numeric? ch)
          (define-values (num-str j)
            (let loopnum ((j i) (acc ""))
              (if (and (< j len)
                       (let ([cj (string-ref s j)])
                         (or (char-numeric? cj) (char=? cj #\.))))
                  (loopnum (add1 j) (string-append acc (string (string-ref s j))))
                  (values acc j))))
          (loop j (cons (cons 'num (string->number num-str)) acc))]
         [else #f])]))
  (loop 0 '()))  

  ;; Evaluation Logic

(define (history-lookup history n)
    (if (and (integer? n) (>= n 1) (<= n (length history)))
        (list-ref (reverse history) (sub1 n))
        #f))

(define (int-div a b)
    (if (zero? b)
        'div-by-zero
        (exact->inexact (quotient (inexact->exact (truncate a))
                                  (inexact->exact (truncate b))))))

(define (eval-expr tokens history)
  (cond
    [(null? tokens) #f]
    [else
     (let ([tok (car tokens)]
           [rest (cdr tokens)])
       (match tok
         [(cons 'num n) (list n rest)]
         [(cons 'hist n)
          (let ([val (history-lookup history n)])
            (if val (list val rest) #f))]
         ['neg
          (let ([r (eval-expr rest history)])
            (and r (list (- (car r)) (cadr r))))]
         ['plus
          (let* ([r1 (eval-expr rest history)]
                 [r2 (and r1 (eval-expr (cadr r1) history))])
            (and r1 r2 (list (+ (car r1) (car r2)) (cadr r2))))]
         ['mult
          (let* ([r1 (eval-expr rest history)]
                 [r2 (and r1 (eval-expr (cadr r1) history))])
            (and r1 r2 (list (* (car r1) (car r2)) (cadr r2))))]
         ['div
          (let* ([r1 (eval-expr rest history)]
                 [r2 (and r1 (eval-expr (cadr r1) history))])
            (and r1 r2
                 (let ([v2 (car r2)])
                   (if (zero? v2)
                       #f
                       (list (int-div (car r1) v2) (cadr r2))))))]
         [_ #f]))]))

;;REPL loop
(define (print-result id value)
    (display (format "~a: " id))
    (display (real->double-flonum value))
    (newline))

(define (print-error)
    (displayln "Error: Invalid Expression"))

(define (repl history)
  (when interactive?
    (display "> ")
    (flush-output))
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]
    [(string=? (string-trim line) "quit") (void)]
    [else
     (define tokens (string->tokens (string-trim line)))
     (if (not tokens)
         (begin (print-error)
                (repl history))
         (let ([res (eval-expr tokens history)])
           (if (and res (null? (cadr res)))
               (let* ([val (car res)]
                      [new-hist (cons val history)]
                      [id (length (reverse new-hist))])
                 (print-result id val)
                 (repl new-hist))
               (begin (print-error)
                      (repl history)))))]))


;;Main entry
(define (main)
    (repl '()))

(module+ main
  (main))

