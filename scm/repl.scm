(define (get-repl-expression-stream)
  (let* ((stdin (field (java.lang.System '()) in))
         (reader (new java.io.BufferedReader
                      (java.io.Reader (new java.io.InputStreamReader
                                           (java.io.InputStream stdin))))))
    (let loop ((acc (call (java.io.BufferedReader reader) readLine)))
      (begin
        (define error #f)
        (let ((expression (try
                           (lambda ()
                             (read (new java.io.PushbackInputStream
                                        (java.io.InputStream (new java.io.ByteArrayInputStream
                                                                  (byte[] (call (java.lang.String acc)
                                                                                getBytes
                                                                                (java.lang.String "UTF8"))))))))
                           (lambda (exn)
                             (set! error #t)))))
          (if error
            (loop (string-append acc (call (java.lang.Object #\newline) toString) (call (java.io.BufferedReader reader) readLine)))
            expression))))))

(define (read-all input-port)
  (let loop ((acc '())
	     (s-exp (read input-port)))
    (if (not (eof-object? s-exp))
      (loop (cons s-exp acc) (read input-port))
      (reverse acc))))

(define repl-running #f)

(define (make-repl-procedures context)
  (new-map 'eval
           (new-ref
            (make-host-procedure
             (lambda (s-exp)
               (eval (macroexpand s-exp context) context))))

           'macroexpand
           (new-ref
            (make-host-procedure
             (lambda (s-exp)
               (macroexpand s-exp context))))

           'macroexpand-1
           (new-ref
            (make-host-procedure
             (lambda (s-exp)
               (macroexpand-1 s-exp context (new-ref #f)))))

           'read
           (new-ref
            (make-host-procedure
             (lambda (input-port)
               (read input-port))))

           'read-all
           (new-ref
            (make-host-procedure
             (lambda (input-port)
               (read-all input-port))))

           'raise
           (new-ref
            (make-host-procedure
             (lambda (exn)
               (raise exn))))

           'load
           (new-ref
            (make-host-procedure
             (lambda (filename)
               (let* ((stream (open-input-file filename))
                      (exps (read-all stream))
                      (expanded (macroexpand `(begin ,@exps) context))
                      (result (eval expanded context)))
                 (close-input-port stream)
                 result))))

           'try
           (new-ref
            (make-host-procedure
             (lambda (fn catcher)
               ;; Make sure call to try are not tail call optimized.
               (let ((result (try
                              (lambda () (guest-apply fn '()))
                              (lambda (exn) (guest-apply catcher (list exn))))))
                 #!void
                 result))))

           'exit
           (new-ref
            (make-host-procedure
             (lambda ()
               (set! repl-running #f))))))

(define (make-base-context)
  (let ((context (new-map 'env (new-ref builtin-procedures)
                          'macros (new-ref (new-map)))))
    (ref-set! (map-get context 'env)
              (map-merge (ref-deref (map-get context 'env)) (make-repl-procedures context)))
    context))

(define (meval s-exp context)
  (eval (macroexpand `(begin ,s-exp) context) context))

(define (run-repl prompt)
  (let ((context (make-base-context)))
    (set! repl-running #t)
    (display (string-append prompt " "))
    (let loop ((s-exp (get-repl-expression-stream)))
      (if (not (eof-object? s-exp))
        (begin
          (display
           (try
            (lambda ()
              (meval s-exp context))
            (lambda (exn)
              exn)))
          (newline)
          (display (string-append prompt " "))
          (if repl-running
            (loop (get-repl-expression-stream))))))))
