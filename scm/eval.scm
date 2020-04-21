;; params: proc
(define (guest-apply proc args)
  (cond
   ;; Simply call the host's apply on host's procedures.
   ((host-procedure? proc)
    (apply (get-tagged-object-value proc) args))

   ((guest-procedure? proc)
    (let* ((proc (get-tagged-object-value proc))
           (env
            ;; Create a new environment from provided arguments & the closure's captured environment.
            (let loop ((acc (map-get proc 'closure))
                       (arg-names (map-get proc 'args))
                       (arg-values args))
              (cond
               ((and (null? arg-names) (null? arg-values)) acc)
               ;; Associate a provided argument value with its name stored in the procedure structure.
               ((and (pair? arg-names) (pair? arg-values))
                (loop (map-add acc (car arg-names) (new-ref (car arg-values))) (cdr arg-names) (cdr arg-values)))
               ;; Case where the lambda arguments was defined with a rest notation or a single symbol.
               ((symbol? arg-names) (map-add acc arg-names (new-ref arg-values)))
               (else (raise (new-map 'message "Invalid number of arguments given"
                                     'proc proc)))))))
      ;; Evaluate the procedure's body with the new environment.
      (eval-program (map-get proc 'body) (new-map 'env (new-ref env)))))))

(define builtin-procedures
  (new-map '+ (new-ref (make-host-procedure +))
           '- (new-ref (make-host-procedure -))
           '* (new-ref (make-host-procedure *))
           '> (new-ref (make-host-procedure >))
           '>= (new-ref (make-host-procedure >=))
           '< (new-ref (make-host-procedure <))
           '<= (new-ref (make-host-procedure <=))
           'raise (new-ref (make-host-procedure raise))
           'null? (new-ref (make-host-procedure null?))
           'apply (new-ref (make-host-procedure guest-apply))
           'debug (new-ref (make-host-procedure debug))))

;; Extracts all the names introduced by a (define ...) syntax.
;; Will include the defines of a nested (begin ...) syntax.
(define (collect-defines s-exps)
  (let loop ((s-exps s-exps) (acc (new-set)))
    (if (null? s-exps)
      acc
      (match (car s-exps)
        ((define (,name . ,_1) . ,_2) when: (symbol? name)
         (loop (cdr s-exps) (set-add acc name)))
        ((define ,name ,_)
         (loop (cdr s-exps) (set-add acc name)))
        ((begin . ,inner-s-exps)
         (loop (cdr s-exps) (loop inner-s-exps acc)))
        (,_ (loop (cdr s-exps) acc))))))

(define (get-string-or-symbol-string x)
  (if (symbol? x) (symbol->string x) x))

(define (list->class-array xs)
  (let* ((len (length xs))
         (array (call (java.lang.reflect.Array '())
                      newInstance
                      (java.lang.Class (typeof "java.lang.Class"))
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

(define (eval-new-object type-name type-hinted-args context)
  (let* ((type (typeof (get-string-or-symbol-string type-name)))
         (args (map (lambda (type-hinted-arg)
                      (match type-hinted-arg
                        ((,_ ,arg-s-exp) (eval arg-s-exp context))))
                    type-hinted-args))
         (arg-types (map (lambda (type-hinted-arg)
                           (match type-hinted-arg
                             ((,type-name ,_) (typeof (get-string-or-symbol-string type-name)))))
                         type-hinted-args))
         (constructor (call (java.lang.Class type)
                            getConstructor
                            ("[Ljava.lang.Class;" (list->class-array arg-types)))))
    (call (java.lang.reflect.Constructor constructor)
          newInstance
          ("[Ljava.lang.Object;" (list->array args)))))

(define (eval-method-call target type-name method-name type-hinted-args context)
  (let* ((obj (eval target context))
         (args-values (map (lambda (type-hinted-arg)
                             (match type-hinted-arg
                               ((,_ ,s-exp) (eval s-exp context))))
                           type-hinted-args))
         (type (typeof type-name))
         (arg-types (map (lambda (type-hinted-arg)
                            (match type-hinted-arg
                             ((,type-name ,_) (typeof (get-string-or-symbol-string type-name)))))
                          type-hinted-args))
         (method (call (java.lang.Class type)
                       getMethod
                       (java.lang.String method-name)
                       ("[Ljava.lang.Class;" (list->class-array arg-types)))))
    (call (java.lang.reflect.Method method)
          invoke
          (java.lang.Object obj)
          ("[Ljava.lang.Object;" (list->array args-values)))))

(define (eval-field-access target type-name field-name context)
  (let* ((obj (eval target context))
         (type (typeof type-name))
         (field (call (java.lang.Class type)
                       getField
                       (java.lang.String field-name))))
    (call (java.lang.reflect.Field field)
          get
          (java.lang.Object obj))))

(define (eval-program s-exps context)
  ;; Evaluates all s-exps in sequence, returning the value of the last one.
  (define (loop s-exps context)
    (if (pair? (cdr s-exps))
      (begin
        (eval-sexp (car s-exps) context)
        (loop (cdr s-exps) context))
      (eval-sexp (car s-exps) context)))

  ;; This is used to evaluate nested (begin ...) syntaxes directly, as
  ;; their defined names were already included in the environment.
  (define (eval-sexp s-exp context)
    (match s-exp
      ((begin . ,s-exps) (loop s-exps context))
      (,_ (eval s-exp context))))

  (let ((new-env
         ;; Locally extend the environment with names introduced by
         ;; (define ...) syntaxs in the s-exp list and recursively
         ;; in all nested (begin ...) syntaxs. The name are initally
         ;; bound to #!void, and will have their value set when actually
         ;; evaluating the (define ...).
         (let loop ((env (ref-deref (map-get context 'env)))
                    (new-defines (set->list (collect-defines s-exps))))
           (if (null? new-defines)
             env
             (loop (map-add env (car new-defines) (new-ref #!void))
                   (cdr new-defines))))))
    (ref-set! (map-get context 'env) new-env)
    (loop s-exps context)))

;; Build up the set of the names defined in a lambda's argument list.
(define (get-args-defined-names args)
  (let loop ((acc (new-set)) (args args))
    (cond
     ((null? args) acc)
     ((pair? args) (loop (set-add acc (car args)) (cdr args)))
     ((symbol? args) (set-add acc args))
     (else (raise "Ill formed lambda arguments")))))

(define (get-free-vars-in-quasiquoted-pair acc bound-vars free-vars s-exp)
  (match s-exp
    ((,car- . ,cdr-)
     (let ((x (get-free-vars-in-quasiquoted car- bound-vars)))
       (let? (((,car-free-vars ,new-car) x))
         (get-free-vars-in-quasiquoted-pair (cons new-car acc)
                                            bound-vars
                                            (set-union car-free-vars free-vars)
                                            cdr-))))

    (,value
     (let? (((,value-free-vars ,new-value) (get-free-vars-in-quasiquoted value bound-vars)))
       (list (set-union value-free-vars free-vars)
             (let loop ((acc value) (xs acc))
               (if (null? xs)
                 acc
                 (loop (cons (car xs) acc) (cdr xs)))))))))

(define (get-free-vars-in-quasiquoted s-exp bound-vars)
  (match s-exp
    ((,unquote-symbol ,unquoted) when: (eq? unquote-symbol 'unquote)
     (let? (((,free-vars ,new-unquoted) (get-free-vars unquoted bound-vars)))
       (list free-vars (list 'unquote new-unquoted))))
    ((,unquote-splicing-symbol ,unquote-spliced) when: (eq? unquote-splicing-symbol 'unquote-splicing)
     (let? (((,free-vars ,new-unquote-spliced) (get-free-vars unquote-spliced bound-vars)))
       (list free-vars (list 'unquote-splicing new-unquote-spliced))))
    ((,x . ,xs)
     (get-free-vars-in-quasiquoted-pair '() bound-vars (new-set) `(,x . ,xs)))
    (,x (list (new-set) x))))

(define (get-free-vars-from-lists s-exps bound-vars)
  (let? (((,free-vars ,new-s-exps-rev)
          (reduce (lambda (acc s-exp)
                    (let? (((,free-vars ,new-s-exps-rev) acc)
                           ((,new-free-vars ,new-s-exp) (get-free-vars s-exp bound-vars)))
                      (list (set-union free-vars new-free-vars)
                            (cons new-s-exp new-s-exps-rev))))
                  (list (new-set) '())
                  s-exps)))
    (list free-vars (reverse new-s-exps-rev))))

;; Extract all the "free-vars" in a s-exp, that is names that are
;; referred to, but not defined in the s-exp.
(define (get-free-vars s-exp bound-vars)
  (match s-exp
    ;; Include a name in the result only if it's not locally defined.
    (,symbol
     when: (symbol? symbol)
     (list (if (set-contains bound-vars symbol)
             (new-set)
             (new-set symbol))
           symbol))

    (,proc
     when: (guest-procedure? proc)
     (list (set-except (map-get proc 'free-vars) bound-vars)
           proc))

    ((if ,condition ,true-branch ,false-branch)
     (let? (((,condition-free-vars ,new-condition)
             (get-free-vars condition bound-vars))
            ((,true-branch-free-vars ,new-true-branch)
             (get-free-vars true-branch bound-vars))
            ((,false-branch-free-vars ,new-false-branch)
             (get-free-vars false-branch bound-vars)))
       (list (set-union condition-free-vars true-branch-free-vars false-branch-free-vars)
             `(if ,new-condition
                ,new-true-branch
                ,new-false-branch))))

    ((if ,condition ,true-branch)
     (let? (((,condition-free-vars ,new-condition)
             (get-free-vars condition bound-vars))
            ((,true-branch-free-vars ,new-true-branch)
             (get-free-vars true-branch bound-vars)))
       (list (set-union condition-free-vars true-branch-free-vars)
             `(if ,new-condition
                ,new-true-branch))))

    ((begin . ,body)
     (let? ((,new-defines (collect-defines body))
            (,new-bound-vars (set-union bound-vars new-defines))
            ((,free-vars ,new-body) (get-free-vars-from-lists body new-bound-vars)))
       (list free-vars `(begin ,@new-body))))

    ((new ,type-name . ,type-hinted-args)
     (let? (((,free-vars ,new-args)
             (get-free-vars-from-lists (map cadr type-hinted-args) bound-vars)))
       (list free-vars
             `(new ,type-name
                   ,@(map (lambda (type-hint new-arg)
                            `(,type-hint ,new-arg))
                          (map car type-hinted-args)
                          new-args)))))

    ((call (,type-name ,target) ,method-name . ,type-hinted-args)
     (let? (((,args-free-vars ,new-args)
             (get-free-vars-from-lists (map cadr type-hinted-args) bound-vars))
            ((,target-free-vars ,new-target)
             (get-free-vars target bound-vars)))
       (list (set-union args-free-vars target-free-vars)
             `(call (,type-name ,new-target)
                    ,method-name
                   ,@(map (lambda (type-hint new-arg)
                            `(,type-hint ,new-arg))
                          (map car type-hinted-args)
                          new-args)))))

    ((field (,type-name ,target) ,field-name)
     (let? (((,free-vars ,new-target) (get-free-vars target bound-vars)))
       (list free-vars
             `(field (,type-name ,new-target) ,field-name))))

    ;; Names defined in a lambda's arguments are locally defined
    ;; in the scope of the lambda's body.
    ((lambda ,args . ,body)
     (let* ((locally-bound-vars (set-union (get-args-defined-names args)
                                           (collect-defines body)))
            (lambda-w-free-vars (make-lambda-w-free-vars locally-bound-vars args body)))
       (list (set-except (lambda-w-free-vars-free-vars lambda-w-free-vars)
                         bound-vars)
             lambda-w-free-vars)))

    ((define (,fn-name . ,args) . ,body)
     ;; Note: locally-bound-vars does not include fn-name because it must be captured by
     ;; the closure.
     (let* ((locally-bound-vars (set-union (get-args-defined-names args)
                                           (collect-defines body)))
            (lambda-w-free-vars (make-lambda-w-free-vars locally-bound-vars args body)))
       (list (set-except (lambda-w-free-vars-free-vars lambda-w-free-vars)
                         bound-vars)
             `(define ,fn-name ,lambda-w-free-vars))))

    ((define ,name ,value)
     (let? (((,free-vars ,new-value)
             (get-free-vars value bound-vars)))
       (list free-vars
             `(define ,name ,new-value))))

    ((quote ,quoted) (list (new-set) `(quote ,quoted)))

    ((quasiquote ,body)
     (let? (((,free-vars ,new-body)
             (get-free-vars-in-quasiquoted body bound-vars)))
       (list free-vars
             `(quasiquote ,new-body))))

    ((set! ,name ,s-exp)
     when: (symbol? name)
     (let? (((,free-vars ,new-s-exp)
             (get-free-vars s-exp bound-vars)))
       (list (set-union
              (if (set-contains bound-vars name)
                (new-set)
                (new-set name))
              free-vars)
             `(set! ,name ,new-s-exp))))

    ((,proc . ,args)
     (let? (((,proc-free-vars ,new-proc) (get-free-vars proc bound-vars))
            ((,args-free-vars ,new-args) (get-free-vars-from-lists args bound-vars)))

       (list (set-union proc-free-vars args-free-vars)
             `(,new-proc ,@new-args))))

    (,x (list (new-set) x))))

(define (make-lambda-w-free-vars bound-vars args body)
  (let? (((,free-vars ,new-body)
          (get-free-vars-from-lists body bound-vars)))
    (make-tagged-object "lambda-w-free-vars"
                        (new-map 'lambda `(lambda ,args ,@new-body)
                                 'free-vars free-vars))))

(define (lambda-w-free-vars? x)
  (is-tagged-object? x "lambda-w-free-vars"))

(define (lambda-w-free-vars-lambda x)
  (map-get (get-tagged-object-value x) 'lambda))

(define (lambda-w-free-vars-free-vars x)
  (map-get (get-tagged-object-value x) 'free-vars))

(define (eval-lambda args body context)
  (let? (((,_ ,lambda-w-free-vars) (get-free-vars `(lambda ,args ,@body) (new-set))))
    (eval-lambda-w-free-vars lambda-w-free-vars context)))

(define (eval-lambda-w-free-vars lambda-w-free-vars context)
  (let* ((env (ref-deref (map-get context 'env)))
         (free-vars (lambda-w-free-vars-free-vars lambda-w-free-vars))
         (lambda_ (lambda-w-free-vars-lambda lambda-w-free-vars))
         (captured-env
          (apply new-map (reduce (lambda (acc var)
                                   (if (not (map-contains-key? env var))
                                     (raise (new-map 'message "Unbound name captured by closure"
                                                     'var-name var
                                                     'lambda lambda-w-free-vars
                                                     'context context))
                                     (append (list var (map-get env var)) acc)))
                                 '()
                                 (set->list free-vars)))))
    (let? (((lambda ,args . ,body) lambda_))
     (make-guest-procedure
      (new-map 'closure captured-env
               'body body
               'args args
               'free-vars free-vars)))))

(define (map-quasiquoted s-exp fn)
  (match s-exp
    ((,unquote-symbol ,unquoted) when: (eq? 'unquote unquote-symbol) (fn unquoted))
    (,pair when: (pair? pair) (map-quasiquoted-pair pair fn))
    (,_ s-exp)))

(define (map-quasiquoted-pair s-exps fn)
  (let loop ((acc '()) (s-exps s-exps))
    (match s-exps
      (() (reverse acc))
      (((,sym ,unquote-spliced) . ,rest) when: (eq? sym 'unquote-splicing)
       (loop (reduce (flip cons) acc (fn unquote-spliced)) rest))
      ((,s-exp . ,rest)
       (loop (cons (map-quasiquoted s-exp fn) acc) rest))
      (,s-exp (reduce (lambda (acc x) (cons x acc)) (map-quasiquoted s-exp fn) acc)))))

(define (eval-quasiquoted s-exp context)
  (map-quasiquoted s-exp (lambda (s-exp)
                           (eval s-exp context))))

(define (assert-name-defined name context)
  (let ((env (ref-deref (map-get context 'env))))
    (if (not (map-contains-key? env name))
      (raise (string-append "Undefined name " (symbol->string name))))))

(define (eval s-exp context)
  (match s-exp
    (,symbol
     when: (symbol? symbol)
     (begin
       (assert-name-defined symbol context)
       (-> (map-get context 'env)
           (ref-deref)
           (map-get symbol)
           (ref-deref))))

    ((if ,condition ,true-branch)
     (if (eval condition context)
       (eval true-branch context)
       #!void))

    ((if ,condition ,true-branch ,false-branch)
     (if (eval condition context)
       (eval true-branch context)
       (eval false-branch context)))

    ((quote ,s-exp) s-exp)

    ((quasiquote ,s-exp)
     (eval-quasiquoted s-exp context))

    ((set! ,var ,s-exp)
     (begin
       (assert-name-defined var context)
       (let ((ref (-> (map-get context 'env) (ref-deref) (map-get var))))
         (ref-set! ref (eval s-exp context)))))

    ((define (,name . ,args) . ,body)
     when: (symbol? name)
     (begin
       (assert-name-defined name context)
       (let ((ref (map-get (ref-deref (map-get context 'env)) name)))
         (ref-set! ref (eval `(lambda ,args ,@body) context)))))

    ((define ,name ,value)
     when: (symbol? name)
     (begin
       (assert-name-defined name context)
       (let ((ref (map-get (ref-deref (map-get context 'env)) name)))
         (ref-set! ref (eval value context)))))

    ((define-macro ,_1 . ,_2)
     '())

    ((new ,type-name . ,type-hinted-args)
     (eval-new-object (get-string-or-symbol-string type-name)
                      type-hinted-args
                      context))

    ((call (,type-name ,target) ,method-name . ,type-hinted-args)
     (eval-method-call target
                       (get-string-or-symbol-string type-name)
                       (get-string-or-symbol-string method-name)
                       type-hinted-args
                       context))

    ((field (,type-name ,target) ,field-name)
     (eval-field-access target
                        (get-string-or-symbol-string type-name)
                        (get-string-or-symbol-string field-name)
                        context))

    ((begin . ,s-exps)
     (eval-program s-exps context))

    ((lambda ,args . ,body)
     (eval-lambda args body context))

    (,lambda-w-free-vars
     when: (lambda-w-free-vars? lambda-w-free-vars)
     (eval-lambda-w-free-vars lambda-w-free-vars context))

    ((,proc . ,args)
     (let* ((proc (eval proc context))
            (args (map (lambda (s-exp)
                         (eval s-exp context))
                       args)))
       (guest-apply proc args)))

    (,symbol
     when: (symbol? symbol)
     (begin
       (assert-name-defined symbol context)
       (ref-deref (map-get (ref-deref (map-get context 'env)) symbol))))

    (,_ s-exp)))
