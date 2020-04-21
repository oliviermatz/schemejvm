(define (whitespace? char)
  (let ((code (char->integer char)))
    (or (eqv? code 32) 
	(eqv? code 10)
	(eqv? code 13)
	(eqv? code 9))))

(define (digit? char)
  (let ((code (char->integer char)))
    (and
     (>= code (char->integer #\0))
     (<= code (char->integer #\9)))))

(define (skip-whitespaces input-port)
  (if (and (not (input-port-eof? input-port))
	   (whitespace? (peek-char input-port)))
    (begin
      (read-char input-port)
      (skip-whitespaces input-port))))

(define (skip-to-end-of-line input-port)
  (if (not (input-port-eof? input-port))
    (let loop ((c (read-char input-port)))
      (if (and (not (input-port-eof? input-port))
	       (not (eqv? #\newline c)))
	(loop (read-char input-port))))))

(define (skip-whitespaces-and-comments input-port)
  (let loop ()
    (skip-whitespaces input-port)
    (if (not (input-port-eof? input-port))
      (begin
	(if (eqv? (peek-char input-port) #\;)
	  (skip-to-end-of-line input-port))
	(if (not (input-port-eof? input-port))
          (let ((c (peek-char input-port)))
            (if (or (whitespace? c) (eqv? #\; c))
              (loop))))))))

(define (letter? c)
  (let ((code (char->integer c)))
    (or
     (and
      (>= code (char->integer #\a))
      (<= code (char->integer #\z)))
     (and
      (>= code (char->integer #\A))
      (<= code (char->integer #\Z))))))

(define (first-symbol-char? c)
  (or (letter? c)
      (member? c '(#\@ #\! #\+ #\- #\* #\= #\_ #\? #\< #\> #\:))))

(define (rest-symbol-char? c)
  (or (letter? c)
      (digit? c)
      (member? c '(#\. #\! #\+ #\- #\* #\= #\_ #\@ #\? #\< #\> #\# #\: #\[ #\]))))

(define (read-symbol input-port acc)
  (if (first-symbol-char? (peek-char input-port))
    (let loop ((acc (cons (read-char input-port) acc)))
      (if (and (not (input-port-eof? input-port))
               (rest-symbol-char? (peek-char input-port)))
        (loop (cons (read-char input-port) acc))
        (string->symbol (list->string (reverse acc)))))))

(define (read-number input-port)
  (let ((left-part (read-integer-string input-port)))
    (if (and (not (input-port-eof? input-port))
	     (eqv? (peek-char input-port) #\.)) 
      (begin
	(read-char input-port)
	(let ((right-part (read-integer-string input-port)))
	  (string->number (string-append left-part "." right-part))))
      (string->integer left-part))))

(define (read-integer-string input-port)
  (let loop ((acc '()))
    (if (and (not (input-port-eof? input-port))
	     (digit? (peek-char input-port))) 
      (loop (cons (read-char input-port) acc))
      (list->string (reverse acc)))))

(define (read-string input-port)
  (if (eqv? #\" (peek-char input-port))
    (begin
      (read-char input-port)
      (let loop ((acc '()))
	(if (eqv? #\" (peek-char input-port))
	  (begin
	    (read-char input-port)
	    (list->string (reverse acc)))
	  (loop (cons (read-char input-port) acc)))))))

(define (read-quoted input-port)
  (if (eqv? #\' (peek-char input-port))
    (begin
      (read-char input-port)
      (list 'quote (read-inner input-port)))))

(define (read-quasiquoted input-port)
  (if (eqv? #\` (peek-char input-port))
    (begin
      (read-char input-port)
      (list 'quasiquote (read input-port)))))

(define (read-unquoted input-port)
  (if (eqv? #\, (peek-char input-port))
    (begin
      (read-char input-port)
      (if (eqv? #\@ (peek-char input-port))
	(begin
	  (read-char input-port)
	  (list 'unquote-splicing (read-inner input-port)))
	(list 'unquote (read-inner input-port))))))

(define (read-special-hashtag input-port start-char)
  (let ((sym (read-symbol input-port (list start-char))))
    (cond
     ((eqv? sym '!void) #!void)
     ((eqv? sym '!eof) #!eof)
     (else (raise "Ill formed # token")))))

(define (read-hashtag input-port)
  (if (eqv? #\# (peek-char input-port))
    (begin
      (read-char input-port)
      (let ((c (read-char input-port)))
	(cond
	 ((eqv? c #\\) (read-character input-port))
	 ((eqv? c #\t) #t)
	 ((eqv? c #\f) #f)
	 (else (read-special-hashtag input-port c)))))))

(define (delimiter? c) 
  (or (whitespace? c) (eqv? c #\() (eqv? c #\))))

(define (read-character input-port)
  (let ((char (read-char input-port)))
    (if (not (delimiter? (peek-char input-port)))
      (let ((sym (read-symbol input-port '())))
	(cond
	 ((and (eqv? char #\s) (eqv? sym 'pace)) #\space)
	 ((and (eqv? char #\n) (eqv? sym 'ewline)) #\newline)))
      char)))

(define (read-sexps-list input-port)
  (read-char input-port)
  (let loop ((acc '()))
    (skip-whitespaces-and-comments input-port)
    (let ((current (peek-char input-port)))
      (cond 
       ((eqv? current #\))
        (begin
          (read-char input-port)
          (reverse acc)))
       ((eqv? current #\.)
        (begin
          (read-char input-port)
          (skip-whitespaces-and-comments input-port)
          (let ((last (read-inner input-port)))
            (skip-whitespaces-and-comments input-port)
            (if (not (eqv? (read-char input-port) #\)))
              (raise "Ill formed dot expression"))
            (reduce (lambda (acc x) (cons x acc)) last acc))))      
       (else (loop (cons (read-inner input-port) acc)))))))

(define (read-minus input-port)
  (read-char input-port)
  (let ((c (peek-char input-port)))
    (cond
     ((digit? c)
      (let ((number (read-number input-port)))
	(if (integer? number)
	  (* number -1)
	  (* number -1.0))))
     ((rest-symbol-char? c)
      (read-symbol input-port (list #\-)))
     (else '-))))

(define (read-inner input-port)
  (skip-whitespaces-and-comments input-port)  
  (if (input-port-eof? input-port)
    #!eof
    (let ((c (peek-char input-port)))
      (cond 
       ((eqv? c #\() (read-sexps-list input-port))
       ((eqv? c #\-) (read-minus input-port))       
       ((digit? c) (read-number input-port))
       ((first-symbol-char? c) (read-symbol input-port '()))
       ((eqv? c #\#) (read-hashtag input-port))
       ((eqv? c #\") (read-string input-port))
       ((eqv? c #\') (read-quoted input-port))
       ((eqv? c #\`) (read-quasiquoted input-port))
       ((eqv? c #\,) (read-unquoted input-port))
       (else
	(begin
	  (raise (string-append "Unexpected character code " (number->string (char->integer c)))) 
	  #f))))))

(define (read stream)
  (read-inner stream))

