#lang racket
(require racket/tcp)
(require racket/cmdline)

; n : int, kB
(define (upload n in out)
  (write-bytes (bytes 1) out)
  (flush-output)
  (write-string (number->string n) out)
  (newline out)
  (flush-output out)
  (time-expr (write-bytes (make-bytes (* 1000 n) 6) out)))

; n : int, kB
(define (download n in out)
  (write-bytes (bytes 0) out)
  (flush-output)
  (write-string (number->string n) out)
  (newline out)
  (flush-output out)
  (time-expr (read-bytes (* 1000 n) in)))

(define-syntax-rule (time-expr e) 
  (let-values ([(a b c d) (time-apply (lambda () e) null)])
   c))

(define (run-a-test proc in out)
  (define-values (b t) (find-size (lambda (x)  (time-expr (proc x in out))) 10 1 1)) 
    (let* ([n (round (/ 20000 (max t 1)))]
           [values (map
                    (lambda (x)
                      (display #\return)
                      (display (round (/ (* 100 (+ x 1)) n)))
                      (display "%")
                      (proc b in out))
                    (range 0 n))])
           (report values b)
           ))

(define (run-test hostname port)
  (define-values (in out) (tcp-connect hostname port))
  (displayln "Testing Download")
  (run-a-test download in out)
  (displayln "Testing Upload")
  (run-a-test upload in out)
  (close-input-port in)
  (close-output-port out))

(define (report values b)
  (let* ([trimmed (trim values)]
         [average (round (/ (foldr + 0 trimmed) (length trimmed)))]
         [speed (exact->inexact (/ (round (/ (* 800 b) average)) 100))])
      (display #\newline)
      (display speed)
      (displayln " Mbps")))

(define (trim values)
  (let* ([sorted (sort values <=)]
         [n (length values)]
         [front (round (/ n 10))]
         [back (round (/ n 5))])
    (trim-list sorted front back)))

(define (trim-list list front back)
  (list-tail (reverse (list-tail (reverse list) back)) front))

;finds the size of batches to test
;want b < 20 000
; t < 500
; t > 50
;proc : n -> ms
(define (find-size proc b c t1)
(let ([t (proc b)])
  (cond [(<= 20000 b) (values c t1)]
        [(<= t 200) (find-size proc (round (* b (/ 200 (max t 1)))) b t)]
        [(<= 1000 t) (values c t1)]
        [else (values b t) ])))

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

(define (speedcheck)
 (cond [(get-args) => (lambda (x) (with-handlers ([exn:fail? (lambda (e) (displayln "\nUh oh, something went wrong"))]) (run-test (car x) (cdr x))))]
       [else (displayln "Usage: 'speedcheck [hostname] [port]'. Port is optional, default is 8080")]))

(speedcheck)