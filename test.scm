(use monad)

(define-monad <logger>
  (lambda (a) 
    (let ((p (car a))
          (v (cdr a)))
      (fprintf p "Starting with: ~S\n" v)
      a))
  (lambda (a f)
    (let* ((p (car a))
           (v (cdr a))
           (r (f v)))
      (fprintf p "Calling (~S ~S) returned ~S\n" f v r)
      (cons p r))))

(define (f1 x) (+ x 1))
(define (f2 x) (- x 1))

(define logger-monad 
  (doto-using <logger> 
              (cons (current-output-port) 0) 
              f1 
              f2))

(define id-monad 
  (doto-using <id> 
              0
              f1 
              f2))

(assert (eq? 0 (cdr (run logger-monad)))
        "Did the logger test work?")

;Outputs:
;Starting with: 0
;Calling (#<procedure (f1 x)> 0) returned 1
;Calling (#<procedure (f2 x)> 1) returned 0

(assert (eq? 0 (run id-monad))
        "Did the id test work?")
