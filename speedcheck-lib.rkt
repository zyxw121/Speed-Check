#lang racket
 
(provide
 speedcheck-server
 speedcheck
 run-test
 test
 testu
 get-mega)

(require racket/tcp)
(require racket/cmdline)

(define (start-serve listener)
  (let-values ([(in out) (tcp-accept listener)])
   ; (displayln "client connected")
      (serve in out)
  ;  (displayln "client disconnected")
      (close-input-port in)
      (close-output-port out)))

;serve a client with the given channels
(define (serve in out)
 ; (displayln "waiting for cmd")
  (define cmd (read-byte in))
  ;(display "got cmd: ")
 ; (displayln cmd)
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
      (let ([t (read-bytes (* 1000 1000) in)])
        (displayln (bytes-length t)))
      (displayln "got")
      )
    (write-bytes (bytes 4) out)
    (flush-output out)
    (let ([t (with-time download)])
    t)))

(define (handle-download in out)
 ; (display "Starting DL ")
  (define n (string->number (read-line in)))
 ; (display n)
  ;(displayln " kB")
    (write-bytes (make-bytes (* 1000 n) 0) out)
    (flush-output))
 ; (displayln "DL finished"))


(define (handle-upload in out)
  (display "Starting UL ")
  (define n (string->number (read-line in)))
  (display n)
  (displayln " kB")
  (read-bytes (* 1000 n) in)
  (displayln "UL finished"))

; 0 <= n < 256
(define (request-upload n in out)
  (write-bytes (bytes 1) out)
  (flush-output)
  (write-string (number->string n) out)
  (newline out)
  (flush-output out)
  (define (upload)
      (write-bytes (make-bytes (* 1000 n) 6) out)
    )
  (with-time upload))

; n between 0 and 255, kB
(define (request-download n in out)
;  (display "Starting DL ")
;  (display n)
;  (displayln "kB")
  (write-bytes (bytes 0) out)
  (flush-output)
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
  c)

(define (batch proc n hostname i)
  (let-values ([(in out) (tcp-connect hostname 8080)])
    (define t (proc n in out))
    (close-input-port in)
    (close-output-port out)
    (display #\return)
    (display i)
    t))

(define (run-a-test proc hostname)
;  (displayln hostname)
  (define-values (b t) (find-size (lambda (x)  (with-time (lambda () (batch proc x hostname 0)))) 10 1 1))
    (let ([n (round (/ 20000 t))])
    (display n)
    (display " batches of ")
    (display b)
    (displayln "kB")
    (let* ([values (map (lambda (x) (batch proc b hostname x)) (range 0 n))]
           [trimmed (trim values)]
           [average (round (/ (foldr + 0 trimmed) (length trimmed)))]
           [speed (exact->inexact (/ (round (/ (* 800 b) average)) 100))])
      (displayln trimmed)
      (display average)
      (displayln " ms")
      (display speed)
      (displayln " Mbps")
      ) ))


(define (run-test hostname)
  (displayln "Testing Download")
  (run-a-test request-download hostname)
  (displayln "Testing Upload")
  (run-a-test request-upload hostname)
  )

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
(define (find-size proc b c t1)
(let ([t (proc b)])
  (cond [(<= t 200) (begin
                   (display b)
                   (display "kb in ")
                   (display t)
                   (displayln "ms")
                   (find-size proc (round (* b (/ 200 t))) b t))]
        [(<= 1000 t) (values c t1)]
        [else (begin
                                                               (display b)
                                                               (display "kb in")
                                                               (display t)
                                                               (displayln "ms.")
                                                               (values b t) )])
  ))


(define (test n)
  (let-values ([(in out) (tcp-connect  "46.101.84.95" 8080)])
  (define t (request-download n in out))
    (close-input-port in)
    (close-output-port out)
    t  ))
(define (testu n)
  (let-values ([(in out) (tcp-connect  "localhost" 8080)])
  (define t (request-upload n in out))
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