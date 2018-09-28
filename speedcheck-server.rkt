#lang racket
(require racket/tcp)
(require racket/cmdline)

(define (handle-download in out)
  (define n (string->number (read-line in)))
    (write-bytes (make-bytes (* 1000 n) 0) out)
    (flush-output))

(define (handle-upload in out)
  (define n (string->number (read-line in)))
  (read-bytes (* 1000 n) in)
  void)

(define (accept listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
  (define-values (in out) (tcp-accept listener))
    (thread (lambda () 
              (match (read-byte in)
                [0 (handle-download in out)]
                [1 (handle-upload in out)]
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
  [ stop (serve port)])
  (define (loop)
    (let ([cmd (read-line)])
      (cond [(equal? cmd "quit") (displayln "Quitting")
                            (stop)]
            [else (loop)])))
  (displayln "Started Speed-Check server")  
  (loop)))

(speedcheck-server)