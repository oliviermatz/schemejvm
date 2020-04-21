(define (make-host-procedure fn)
  (make-tagged-object "host-procedure" fn))

(define (host-procedure? fn)
  (is-tagged-object? fn "host-procedure"))

(define (make-guest-procedure fn)
  (make-tagged-object "guest-procedure" fn))

(define (guest-procedure? fn)
  (is-tagged-object? fn "guest-procedure"))

(define (make-preevaled-lambda fn)
  (make-tagged-object "preevaled-lambda" fn))

(define (preevaled-lambda? fn)
  (is-tagged-object? fn "preevaled-lambda"))

