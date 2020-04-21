(define-macro (match x . clauses)
  (define (match-build-matcher target-symbol clause success-cont failure-cont)
    (cond
     ((number? clause)
      `(if (eqv? ,target-symbol ,clause)
         ,success-cont
         ,failure-cont))

     ((string? clause)
      `(if (string=? ,target-symbol ,clause)
         ,success-cont
         ,failure-cont))

     ((symbol? clause)
      `(if (eq? ,target-symbol ',clause)
         ,success-cont
         ,failure-cont))

     ((or (boolean? clause)
          (char? clause))
      `(if (eq? ,target-symbol ,clause)
         ,success-cont
         ,failure-cont))

     ((and (pair? clause)
           (eq? (car clause) 'unquote)
           (pair? (cdr clause))
           (null? (cddr clause)))
      `(let ((,(cadr clause) ,target-symbol))
         ,success-cont))

     ((null? clause)
      `(if (null? ,target-symbol)
         ,success-cont
         ,failure-cont))

     ((and (pair? clause)
           (pair? (car clause))
           (eq? (caar clause) 'unquote-splicing)
           (pair? (cdar clause))
           (null? (cddar clause)))
      `(let (,(cadar clause) ,target-symbol)
         ,success-cont))

     ((pair? clause)
      (let ((car# (gensym 'car)) (cdr# (gensym 'cdr)))
        `(if (pair? ,target-symbol)
           (let ((,car# (car ,target-symbol)))
             ,(let ((cdr-cont
                     `(let ((,cdr# (cdr ,target-symbol)))
                        ,(match-build-matcher cdr# (cdr clause) success-cont failure-cont))))
                (match-build-matcher car# (car clause) cdr-cont failure-cont)))
           ,failure-cont)))))
  

  (let ((target# (gensym 'target))
        (matchers (map (lambda (_) (gensym 'matchers)) clauses))
        (error-handler (gensym 'error-handler)))
    `(let ((,target# ,x))
       ,@(map (lambda (current-matcher next-matcher clause)
                `(define (,current-matcher)
                   ,(match-build-matcher target#
                                         (car clause)
                                         (if (and (eq? (cadr clause) 'when:)
                                                  (pair? (cddr clause)))
                                           `(if ,(caddr clause)
                                              (begin ,@(cdddr clause))
                                              (,next-matcher))
                                           `(begin ,@(cdr clause)))
                                         `(,next-matcher))))
              matchers
              (append (cdr matchers) (list error-handler))
              clauses)
       (define (,error-handler)
         (raise "Match failed"))
       (,(car matchers)))))

