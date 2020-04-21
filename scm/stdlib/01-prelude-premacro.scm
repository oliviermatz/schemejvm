(define (get-type obj)
  (call (java.lang.Object obj) getClass))

(define (typeof typename)
  (call (io.github.matzoliv.schemejvm.runtime.ReflectionUtils '()) getClassForName (java.lang.String typename)))

(define (eqv? x y)
  (call (java.lang.Object x) equals (java.lang.Object y)))

(define = eqv?)

(define eq? eqv?)

(define equal? eqv?)

(define string=? eqv?)

(define (not x)
  (if x #f #t))  

(define (cons x y)
  (new io.github.matzoliv.schemejvm.runtime.Pair (java.lang.Object x) (java.lang.Object y)))

(define (car x)
  (call (io.github.matzoliv.schemejvm.runtime.Pair x) getCar))

(define (cdr x)
  (call (io.github.matzoliv.schemejvm.runtime.Pair x) getCdr))

(define (cadr pair)
  (car (cdr pair)))

(define (caddr pair)
  (car (cdr (cdr pair))))

(define (cddar pair)
  (cdr (cdr (car pair))))

(define (cdar pair)
  (cdr (car pair)))

(define (caadr pair)
  (car (car (cdr pair))))

(define (caar pair)
  (car (car pair)))

(define (cadar pair)
  (car (cdr (car pair))))

(define (cddr pair)
  (cdr (cdr pair)))

(define (cdddr pair)
  (cdr (cdr (cdr pair))))

(define (reverse xs)
  (define (loop acc xs)
    (if (null? xs)
      acc
      (loop (cons (car xs) acc) (cdr xs))))
  (loop '() xs))

(define (append xs-1 xs-2)
  (define (loop acc xs)
    (if (null? xs)
      acc
      (loop (cons (car xs) acc) (cdr xs))))
  (loop xs-2 (reverse xs-1)))

(define (reduce fn acc xs)
  (if (null? xs)
    acc
    (reduce fn (fn acc (car xs)) (cdr xs))))

(define (length xs)
  (define (loop n xs)
    (if (null? xs)
      n
      (loop (+ n 1) (cdr xs))))
  (loop 0 xs))

(define (member? x xs)
  (if (null? xs)
    #f
    (if (eqv? x (car xs))
      #t
      (member? x (cdr xs)))))

(define (any? pred xs)
  (define (loop xs)
    (if (null? xs)
      #f
      (if (pred (car xs))
	#t
	(loop (cdr xs)))))
  (loop xs))

(define (list . xs) xs)

(define (map fn . xs)
  (define (map-one fn xs)
    (define (loop acc xs)
      (if (null? xs)
        (reverse acc)
        (loop (cons (fn (car xs)) acc) (cdr xs))))
    (loop '() xs))
  
  (define (loop acc xs)
    (if (any? null? xs)
      (reverse acc)
      (loop (cons (apply fn (map-one car xs)) acc) (map-one cdr xs))))
  (loop '() xs))

(define (symbol? x)
  (if (null? x)
    #f
    (eqv? (get-type x) (typeof "io.github.matzoliv.schemejvm.runtime.Symbol"))))

(define (boolean? x)
  (if (null? x)
    #f
    (eqv? (get-type x) (typeof "java.lang.Boolean"))))

(define (integer? x)
  (if (null? x)
    #f
    (eqv? (get-type x) (typeof "java.lang.Integer"))))

(define (char? x)
  (if (null? x)
    #f
    (eqv? (get-type x) (typeof "java.lang.Character"))))

(define (string? x)
  (if (null? x)
    #f
    (eqv? (get-type x) (typeof "java.lang.String"))))

(define (number? x)    
  (if (null? x)
    #f
    (if (eqv? (get-type x) (typeof "java.lang.Integer"))
      #t
      (eqv? (get-type x) (typeof "java.lang.Double")))))

(define (eof-object? obj)
  (if (null? obj)
    #f
    (eqv? (get-type obj) (typeof "io.github.matzoliv.schemejvm.runtime.Eof"))))

(define (void? obj)
  (if (null? obj)
    #f
    (eqv? (get-type obj) (typeof "io.github.matzoliv.schemejvm.runtime.Void"))))

(define (pair? x)
  (if (null? x)
    #f
    (eqv? (get-type x) (typeof "io.github.matzoliv.schemejvm.runtime.Pair"))))  

(define (string-append . xs)
  ((lambda (builder)
     (begin
       (define (loop xs)
	 (if (null? xs)
	   (call (java.lang.StringBuilder builder) toString)
	   (begin
	     (call (java.lang.StringBuilder builder) append (java.lang.String (car xs)))
	     (loop (cdr xs)))))
       (loop xs)))
   (new java.lang.StringBuilder)))

(define (substring s start end)
  (call (java.lang.String s) substring (int start) (int end)))

(define (string-length s)
  (call (java.lang.String s) length))

(define (char->integer c)
  (call (io.github.matzoliv.schemejvm.runtime.ReflectionUtils '()) charToInt (char c)))

(define (integer->char n)
  (call (io.github.matzoliv.schemejvm.runtime.ReflectionUtils '()) intToChar (int n)))

(define (string->symbol s)
  (new io.github.matzoliv.schemejvm.runtime.Symbol (java.lang.String s)))

(define (symbol->string s)
  (call (io.github.matzoliv.schemejvm.runtime.Symbol s) getName))

(define (number->string obj) (call (java.lang.Object obj) toString))

(define (number->int n)
  (new java.lang.Integer (int (call (java.lang.Double n) intValue))))

(define (string->number s)
  (call (java.lang.Double '()) parseDouble (java.lang.String s)))

(define (string->integer s)
  (call (java.lang.Integer '()) parseInt (java.lang.String s)))

(define gensym
  (begin
    (define index 0)
    (lambda maybe-sym
      (set! index (+ index 1))
      (string->symbol
       (string-append (if (pair? maybe-sym)
                        (symbol->string (car maybe-sym))
                        "")
                      "#g"
                      (number->string index))))))

(define (list->string chars)
  (define sb (new java.lang.StringBuilder))
  (define (loop xs)
    (if (null? xs)
      (call (java.lang.Object sb) toString)
      (begin
        (call (java.lang.StringBuilder sb)
              append
              (char (call (java.lang.Character (car xs)) charValue)))
        (loop (cdr xs)))))
  (loop chars))

(define (string-times str n)
  (define sb (new java.lang.StringBuilder))
  (define (loop n)
    (if (< n 1)
      (call (java.lang.Object sb) toString)
      (begin
        (call (java.lang.StringBuilder sb)
              append
              (java.lang.String str))
        (loop (- n 1)))))
  (loop n))

(define (string-char-at s n)
  (call (java.lang.String s) charAt (int n)))

(define (string->list s)
  (define (loop acc i)
    (if (< i 0)
      acc
      (loop (cons (string-char-at s i) acc) (- i 1))))
  (loop '() (- (string-length s) 1)))

(define (make-tagged-object tag obj)
  (new io.github.matzoliv.schemejvm.runtime.TaggedObject
       (java.lang.String tag)
       (java.lang.Object obj)))

(define (is-tagged-object? obj tag)
  (if (not (null? obj))
    (if (eqv? (get-type obj) (typeof "io.github.matzoliv.schemejvm.runtime.TaggedObject"))
      (eqv? (call (io.github.matzoliv.schemejvm.runtime.TaggedObject obj) getTag) tag)
      #f)
    #f))

(define (get-tagged-object-value obj)
  (call (io.github.matzoliv.schemejvm.runtime.TaggedObject obj) getObject))

(define (new-ref value)
  (make-tagged-object "ref" value))

(define (ref-deref ref)
  (get-tagged-object-value ref))

(define (ref-swap! ref fn)
  (call (io.github.matzoliv.schemejvm.runtime.TaggedObject ref)
        setObject
        (java.lang.Object
         (fn (call (io.github.matzoliv.schemejvm.runtime.TaggedObject ref)
                   getObject))))
  #!void)

(define (ref-set! ref value)
  (call (io.github.matzoliv.schemejvm.runtime.TaggedObject ref)
        setObject
        (java.lang.Object value))
  #!void)

(define (now)
  (call (java.lang.System '()) currentTimeMillis))

(define (println x)
  (call (java.io.PrintStream (field (java.lang.System '()) out))
        println
        (java.lang.String (call (java.lang.Object x)
                                toString))))
