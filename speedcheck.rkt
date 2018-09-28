#lang racket
(require racket/tcp)
(require racket/cmdline)

; n : int, kB
(define (request-upload n in out)
  (write-bytes (bytes 1) out)
  (flush-output)
  (write-string (number->string n) out)
  (newline out)
  (flush-output out)
  (define (upload)
      (write-bytes (make-bytes (* 1000 n) 6) out))
  (with-time upload))

; n : int, kB
(define (request-download n in out)
  (write-bytes (bytes 0) out)
  (flush-output)
  (write-string (number->string n) out)
  (newline out)
  (flush-output out)
  (define (download)
      (read-bytes (* 1000 n) in))
  (with-time download))

(define (with-time f)
  (define-values (a b c d) (time-apply f null))
  c)

(define (batch proc n hostname)
  (let-values ([(in out) (tcp-connect hostname 8080)])
    (define t (proc n in out))
    (close-input-port in)
    (close-output-port out)
    t))

(define (run-a-test proc hostname)
  (define-values (b t) (find-size (lambda (x)  (with-time (lambda () (batch proc x hostname)))) 10 1 1)) 
    (let* ([n (round (/ 20000 t))]
           [values (map
                    (lambda (x)
                      (display #\return)
                      (display (round (/ (* 100 (+ x 1)) n)))
                      (display "%")
                      (batch proc b hostname))
                    (range 0 n))])
           (report values b)
           ))

(define (report values b)
  (let* ([trimmed (trim values)]
         [average (round (/ (foldr + 0 trimmed) (length trimmed)))]
         [speed (exact->inexact (/ (round (/ (* 800 b) average)) 100))])
      (display #\newline)
      (display speed)
      (displayln " Mbps")))

(define (run-test hostname)
  (displayln "Testing Download")
  (run-a-test request-download hostname)
  (displayln "Testing Upload")
  (run-a-test request-upload hostname))

(define (trim values)
  (let* ([sorted (sort values <=)]
        [n (length values)]
        [front (round (/ n 10))]
        [back (round (/ n 5))])
    (trim-list sorted front back)))

(define (trim-list list front back)
  (list-tail (reverse (list-tail (reverse list) back)) front))

;finds the size of batches to test, returns n such that 50 < proc n < 200
;want b < 20 000
; t < 500
; t > 50
;proc : n -> ms
(define (find-size proc b c t1)
(let ([t (proc b)])
  (cond [(<= 20000 b) (values c t1)]
        [(<= t 200) (find-size proc (round (* b (/ 200 t))) b t)]
        [(<= 1000 t) (values c t1)]
        [else (values b t) ])))

(define (speedcheck)
  (if (= (vector-length (current-command-line-arguments)) 1) 
      (let  ([hostname (vector-ref (current-command-line-arguments) 0)])
        (displayln hostname)
        (run-test hostname))
      (displayln "Bad args")))

(speedcheck)