#lang racket
(require racket/tcp)
(require racket/cmdline)

(define (download in out)
  (displayln"gotDL")
  (define n (string->number (read-line in)))
    (write-bytes (make-bytes (* 1000 n) 0) out)
    (flush-output))

(define (upload in out)
  (define n (string->number (read-line in)))
  (read-bytes (* 1000 n) in)
  void)

(define (accept listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
  (define-values (in out) (tcp-accept listener))
    (displayln "Connected")
    (thread (lambda ()
              (displayln "waiting for cmd")
              (define cmd (read-byte in))
              (display "got cmd: ")
              (displayln cmd)
              (match cmd
                [0 (download in out)]
                [1 (upload in out)]
                [_ void])
              (close-input-port in)
              (close-output-port out))))
  (thread (lambda ()
            (sleep 5)
            (custodian-shutdown-all cust))))

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (speedcheck-server)
  (let* ([port
    (if (and
         (>= (vector-length (current-command-line-arguments)) 1)
         (port-number? (string->number (vector-ref (current-command-line-arguments) 0))))
        (string->number (vector-ref (current-command-line-arguments) 0))
        8080)]
         [stop (serve port)])
  (define (loop)
    (display ">")
    (let ([cmd (read-line)])
      (cond [(equal? cmd "quit")
             (displayln "Quitting")
             (stop)]
            [else (loop)])))
  (displayln "Started Speed-Check server")  
  (loop)))

(speedcheck-server)