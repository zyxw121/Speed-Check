#lang racket
(require racket/tcp)
(require racket/cmdline)

(define current-verbose-out (make-parameter (lambda (x) (void))))
(define current-port (make-parameter 8080))

(define-syntax-rule (time-expr e) 
  (let-values ([(a b c d) (time-apply (lambda () e) null)])
   c))

(define (verbose x) ((current-verbose-out) x))

(define (send-header x n out)
  (write-bytes (bytes x) out)
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

(define (percent x y)
  (string-append
   (number->string (round (/ (* 100 (+ x 1)) y)))
   "%"))
  
(define (display-inline x)
  (display #\return)
  (display x))

(define (report vs b)
  (define (avg-list xs)
    (/ (foldr + 0 xs) (length xs)))
  (let* ([trimmed (trim vs)]
         [avg-time (round (avg-list trimmed))]
         [speed  (/ (* 8 b) avg-time)])
    (display "\n")
    (display (real->decimal-string speed))
    (displayln " Mbps")))

(define (trim vs)
  (define (chop-list list front back)
    (list-tail (reverse (list-tail (reverse list) back)) front))
  (let* ([sorted (sort vs <=)]
         [n (length vs)]
         [front (round (/ n 10))]
         [back (round (/ n 5))])
    (chop-list sorted front back)))

;finds the size of batches to test
;want b < max-b
;min-t < t < max-t
(define (find-size proc b [min-t 200] [max-t 1000] [max-b 20000])
  (define (next n t target)
    (round (* n (/ target (max t 1)))))
  (define (iter n b0 t0)
    (let ([t (time-expr (proc n))])
      (cond [(or (<= max-b n) (<= max-t t)) (values b0 t0)]
            [(<= t min-t) (iter (next n t min-t) n t)]
            [else (values n t) ])))
  (iter b 1 1))

(define (run-test)
  (define (execute proc)
    (define-values (b t) (find-size proc 10)) 
    (verbose b)
    (verbose "kB chunks, expecting ")
    (verbose t)
    (verbose "ms each\n")
    (let* ([n (round (/ 20000 (max t 1)))]
           [progress (lambda (x t)
                       (display-inline (percent x n))
                       (verbose "   ")
                       (verbose x)
                       (verbose " of ")
                       (verbose n)
                       (verbose "   ")
                       (verbose t)
                       (verbose "ms")
                       (display "   ")
                       (display (real->decimal-string (/ (* 8 b) t)))
                       (display "Mbps")
                       t)]
           [values (map
                    (lambda (x)
                       (progress x (time-expr (proc b))))
                    (range 0 n))])
      (report values b)))
  (displayln "Speed-Check version 0.1.0")
  (define-values (in out) (tcp-connect hostname (current-port)))
  (displayln "Testing Download")
  (execute (lambda (x) (download x in out)))
  (displayln "Testing Upload")
  (execute (lambda (x) (upload x in out)))
  (close-input-port in)
  (close-output-port out))

(define hostname
  (command-line
   #:program "Speed-Check"
   #:once-each
   [("-v" "--verbose") "Run with verbose messages"
                       (current-verbose-out display)]
   [("-p" "--port") port
                    "Connect on a specified port"
                    (current-port (string->number port))]
   #:args (hostname)
   hostname))

(with-handlers ([exn:fail:network? (lambda (e) (displayln "\nUh oh, something went wrong with the connection"))]
                [exn:fail? (lambda (e) (displayln "\nUh oh, something went wrong"))])
  (run-test))