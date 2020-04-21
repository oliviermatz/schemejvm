(define (time fn)
  (let ((start (now))
        (result (fn))
        (end (now)))
    (display "Lasted " (- end start) " ms.")
    (newline)
    result))

(define (to-string . xs)
  (apply string-append
         (map (lambda (x)
                (if (null? x)
                  "()"
                  (call (java.lang.Object x) toString)))
              xs)))

(define (display . xs)
  (let ((out (field (java.lang.System '()) out)))
    (let loop ((xs xs))
      (if (pair? xs)
        (begin
          (call (java.io.PrintStream out) print (java.lang.String (to-string (car xs))))
          (loop (cdr xs)))))))

(define (newline)
  (call
   (java.io.PrintStream (field (java.lang.System '()) out))
   println)
  #!void)

(define (open-input-file file-name)
  (new java.io.PushbackInputStream
       (java.io.InputStream (new java.io.FileInputStream (java.lang.String file-name)))))

(define (close-input-port input-port)
  (call (java.io.PushbackInputStream input-port) close))

(define (read-int input-port)
  (call (java.io.PushbackInputStream input-port) read))

(define (peek-int input-port)
  (let ((n (call (java.io.PushbackInputStream input-port) read)))
    (call (java.io.PushbackInputStream input-port) unread (int n))
    n))

(define (read-char input-port)
  (integer->char (read-int input-port)))

(define (peek-char input-port)
  (integer->char (peek-int input-port)))

(define (input-port-eof? input-port)
  (< (call (java.io.PushbackInputStream input-port) available) 1))

(define (open-input-string str)
  (let ((underlying (new java.io.ByteArrayInputStream
                         (byte[] (call (java.lang.String str) getBytes (java.lang.String "UTF8"))))))
   (new java.io.PushbackInputStream (java.io.InputStream underlying))))

(define (list->array xs)
  (let* ((len (length xs))
         (array (call (java.lang.reflect.Array '())
                      newInstance
                      (java.lang.Class (typeof "java.lang.Object"))
                      (int len))))
    (let loop ((i 0) (xs xs))
      (if (< i len)
        (begin
          (call (java.lang.reflect.Array '())
                set
                (java.lang.Object array)
                (int i)
                (java.lang.Object (car xs)))
          (loop (+ i 1) (cdr xs)))))
    array))

(define (new-array class-name len)
  (call (java.lang.reflect.Array '())
        newInstance
        (java.lang.Class (typeof class-name))
        (int len)))

(define (flip fn)
  (lambda args
    (apply fn (reverse args))))

