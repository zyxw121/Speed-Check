#lang racket
 
(provide
 speedcheck-server
 speedcheck)

(require racket/tcp)
(require racket/cmdline)

(define (start-serve listener)
  (let-values ([(in out) (tcp-accept listener)])
    (displayln "client connected")
      (serve in out)
    (displayln "client disconnected")
      (close-input-port in)
      (close-output-port out)))

;serve a client with the given channels
(define (serve in out)
  (displayln "waiting for cmd")
  (define cmd (read-byte in))
  (display "got cmd: ")
  (displayln cmd)
  (match cmd
    [0 (handle-download in out)
               (serve in out)]
    [1 (handle-upload in out)
             (serve in out)]
    [2 null] ; proper exit
    [3 (send-kilo out)]
    [4 (send-mega out)]
    [_ null])
)
; have to handle eof somehow

(define (send-kilo out)
  (write-bytes (make-bytes 1000 0) out)
  (display "sent kilo ")
  (flush-output))

(define (send-mega out)
  (write-bytes (make-bytes (* 1000 1000) 0) out)
  (display "sent mega ")
  (flush-output))


(define (get-kilo hostname)
  (let-values ([(in out) (tcp-connect  hostname 8080)])
    (define (download)
      (read-bytes 1000 in)
      )
    (write-bytes (bytes 3) out)
    (flush-output out)
    (let ([t (with-time download)])
    t)))

(define (get-mega hostname)
  (let-values ([(in out) (tcp-connect  hostname 8080)])
    (define (download)
      (read-bytes (* 1000 1000) in)
      )
    (write-bytes (bytes 4) out)
    (flush-output out)
    (let ([t (with-time download)])
    t)))

(define (handle-download in out)
  (display "Starting DL ")
  (define n (string->number (read-line in)))
  (display n)
  (displayln " kB")
    (write-bytes (make-bytes (* 1000 n) 0) out)
    (flush-output)
  (displayln "DL finished"))


(define (handle-upload in out)
  (for/list ([i (in-range (read-byte in))])
    (read-bytes (* 1000) in)))

; 0 <= n < 256
(define (request-upload n in out)
  (write-bytes (bytes 1) out)
  (write-bytes (bytes n) out)
  (for/list ([i (in-range n)])
    (write-bytes (make-bytes (* 1000) 0) out)))

; n between 0 and 255, kB
(define (request-download n in out)
;  (display "Starting DL ")
;  (display n)
;  (displayln "kB")
  (write-bytes (bytes 0) out)
;    (displayln "sent DL")
  (write-string (number->string n) out)
  (newline out)
;  (displayln "Sent n")
  (flush-output out)
  (define (download)
      (read-bytes (* 1000 n) in)
;      (displayln "done!")
    )
  (with-time download))



(define (speedcheck-server)
  (define listener (tcp-listen 8080))
  (define (my-thread) (start-serve listener))
  (define (loop)  
    (if (tcp-accept-ready? listener) (start-serve listener) null)
    (loop))
  (loop))

(define (with-time f)
  (define-values (a b c d) (time-apply f null))
  b)


(define (download-batch n hostname)
  (let-values ([(in out) (tcp-connect hostname 8080)])
    (define t (request-download n in out))
    (close-input-port in)
    (close-output-port out)
    t))

(define (run-test hostname)
;  (displayln hostname)
  (define-values (b t) (find-size (lambda (x)  (with-time (lambda ()  (download-batch x hostname)))) 10))
    (let ([n (round (/ 8000 t))])
    (display n)
    (display " batches of ")
    (display b)
    (displayln "kB")
    (let* ([values (map (lambda (x) (download-batch b hostname)) (range 0 n))]
           [trimmed (trim values)]
           [average (round (/ (foldr + 0 trimmed) (length trimmed)))]
           [speed (exact->inexact (/ (round (/ (* 800 b) average)) 100))])
      ;(displayln trimmed)
      (display average)
      (displayln "ms")
      (display speed)
      (displayln "Mbps")
      ) ))
  

(define (trim values)
  (let* ([sorted (sort values <=)]
        [n (length values)]
        [front (round (/ n 10))]
        [back (round (/ n 5))])
    (trim-list sorted front back)
    ))

(define (trim-list list front back)
  (list-tail (reverse (list-tail (reverse list) back)) front)
  )

;finds the size of batches to test, returns n such that 50 < proc n < 200
;proc : n -> ms
(define (find-size proc b)
(let ([t (proc b)])
  (if (<= t 100) (find-size proc (round (* b (/ 100 t)))) (values b t) )
  ))


(define (test n)
  (let-values ([(in out) (tcp-connect  "46.101.84.95" 8080)])
  (define t (request-download n in out))
    (close-input-port in)
    (close-output-port out)
    t  
  ))

(define (speedcheck)
  (if (= (vector-length (current-command-line-arguments)) 1) 
      (let  ([hostname (vector-ref (current-command-line-arguments) 0)])
        (displayln hostname)
        (run-test hostname))
      (displayln "Bad args")
      ))