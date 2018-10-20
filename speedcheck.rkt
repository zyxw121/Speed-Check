#lang racket
(require racket/tcp)
(require racket/cmdline)

(define-syntax-rule (time-expr e) 
  (let-values ([(a b c d) (time-apply (lambda () e) null)])
   c))

(define (send-header x n out)
  (write-bytes (bytes x) out)
  (flush-output)
  (write-string (number->string n) out)
  (newline out)
  (flush-output out))

; n : int, kB
(define (upload n in out)
  (send-header 1 n out)
  (write-bytes (make-bytes (* 1000 n) 6) out))

; n : int, kB
(define (download n in out)
  (send-header 0 n out)
  (read-bytes (* 1000 n) in))

(define (run-a-test proc)
  (define-values (b t) (find-size proc 10)) 
    (let* ([n (round (/ 20000 (max t 1)))]
           [values (map
                    (lambda (x)
                      (display-inline (percent x n))
                      (time-expr (proc b)))
                    (range 0 n))])
           (report values b)
           ))

(define (percent x y)
  (string-append
   (number->string (round (/ (* 100 (+ x 1)) y)))
   "%"))
  
(define (display-inline x)
  (display #\return)
  (display x))

(define (run-test hostname port)
  (displayln "Speed-Check version 0.1.0")
  (define-values (in out) (tcp-connect hostname port))
  (displayln "Testing Download")
  (run-a-test (lambda (x) (download x in out)))
  (displayln "Testing Upload")
  (run-a-test (lambda (x) (upload x in out)))
  (close-input-port in)
  (close-output-port out))

(define (avg-list xs)
  (/ (foldr + 0 xs) (length xs)))

(define (report values b)
  (let* ([trimmed (trim values)]
         [avg-time (round (avg-list trimmed))]
         [speed  (/ (* 8 b) avg-time)])
      (display-inline (real->decimal-string speed))
      (displayln " Mbps")))

(define (trim values)
  (let* ([sorted (sort values <=)]
         [n (length values)]
         [front (round (/ n 10))]
         [back (round (/ n 5))])
    (chop-list sorted front back)))

(define (chop-list list front back)
  (list-tail (reverse (list-tail (reverse list) back)) front))

;finds the size of batches to test
;want b < max-b
;min-t < t < max-t
(define (find-size proc b [min-t 200] [max-t 1000] [max-b 20000])
  (define (iter n b0 t0)
    (let ([t (time-expr (proc n))])
      (cond [(or (<= max-b b) (<= max-t t)) (values b0 t0)]
            [(<= t min-t) (iter (next n t min-t) n t)]
            [else (values n t) ])))
  (iter b 1 1))

;assuming n -> t, finds m with m -> target
(define (next n t target)
  (round (* n (/ target (max t 1)))))

(define (get-args)
  (cond [(>= (vector-length (current-command-line-arguments)) 2)
         (cons
          (vector-ref (current-command-line-arguments) 0)
          (string->number (vector-ref (current-command-line-arguments) 1)))]
        [(= (vector-length (current-command-line-arguments)) 1)
         (cons
          (vector-ref (current-command-line-arguments) 0)
          8080)]
        [else #f]
        ))

;(define (handle-args list)
;  )


(define (speedcheck)
 (cond [(get-args) => (lambda (x)
                        (with-handlers ([exn:fail:network? (lambda (e) (displayln "\nUh oh, something went wrong with the connection"))]
                                        ;[exn:fail? (lambda (e) (displayln "\nUh oh, something went wrong"))])
                                        )
                          (run-test (car x) (cdr x))))]
       [else (displayln "Usage: 'speedcheck [hostname]'. Optional arguments:")
             (displayln "-p [port]         default is 8080")
             (displayln "-v                verbose mode")]))

(speedcheck)