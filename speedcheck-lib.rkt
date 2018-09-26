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
    [3 (send-kilo out)
       ;(serve in out)]
       ]
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


(define (get-kilo n)
  (let-values ([(in out) (tcp-connect  "46.101.84.95" 8080)])
    (define (download)    
      (for/list ([i (in-range n)])
        (displayln i)
    (write-bytes (bytes 3) out)
    (flush-output out)
     (read-bytes 1000 in)
      ))

    (let ([t (with-time download)])
    (close-input-port in)
    (close-output-port out)
    t  
  )))

(define (get-mega hostname)
  (let-values ([(in out) (tcp-connect  "46.101.84.95" 8080)])
    (define (download)
      (read-bytes (* 1000 1000)in)
      )
    (write-bytes (bytes 4) out)
    (flush-output out)
    (let ([t (with-time download)])
    t  
  )))

(define (handle-download in out)
  (display "Starting DL ")
  (define n (read-byte in))
  (display n)
  (displayln " kB")
  (for/list ([i (in-range n)])
    (write-bytes (make-bytes (* 1000) 0) out)
    (display "sent kB ")
    (displayln i)
    (flush-output))
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
  (display "Starting DL ")
  (display n)
  (displayln "kB")
  (write-bytes (bytes 0) out)
  (displayln "sent DL")
  (write-bytes (bytes n) out)
  (displayln "Sent n")
  (flush-output out)
  (define (download)
    (for/list ([i (in-range n)])
      (display "round ")
      (display i)
      (display ", ")
    ;  (write-bytes (bytes 0) out)
    ;  (flush-output out)
    ;  (display "sent, ")
      ;(read-bytes (* 1000) in)
      (for/list ([i (in-range 1000)])
        (read-byte in)
        ;(if (divides? 100 (+ i 1)) (displayln (+ i 1)) null))
        (displayln i))
      (display " read, "))
      (displayln "done!")
    )
  (with-time download))

(define (divides? n m)
  (= 0 (remainder m n))
  )

(define (request-downloads n in out)
  (if (<= n 0) 0 (let ([t (request-download (min n 255) in out)])
                     (+ t (request-downloads (- n 255) in out)))))


(define (speedcheck-server)
  (define listener (tcp-listen 8080))
  (define (my-thread) (start-serve listener))
  (define (loop)  
    (if (tcp-accept-ready? listener) (thread my-thread) null)
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
  (displayln hostname)
  (let* ([t (download-batch 10 hostname)]
         [n (round (/ 10000 1))])
    (display t)
    (display "ms    ")
    (display n)
    (display " batches")
    (displayln (map (lambda (x) (download-batch 10 hostname)) (range 0 10))))
  )

(define (test n)
  (let-values ([(in out) (tcp-connect  "46.101.84.95" 8080)])
  (define t (request-downloads n in out))
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