;; seq ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (seq-first x)
  (call (com.github.krukow.clj_lang.ISeq x) first))

(define (seq-rest x)
  (call (com.github.krukow.clj_lang.ISeq x) next))

(define (seq-empty? x)
  (or
   (null? x)
   (eqv? x (call (com.github.krukow.clj_lang.IPersistentCollection x) empty))))

(define (to-seq x)
  (call (com.github.krukow.clj_lang.Seqable x) seq))

(define (seq-foreach fn seq)
  (let loop ((seq (to-seq seq)))
    (if (not (seq-empty? seq))
      (begin
        (fn (seq-first seq))         
        (loop (seq-rest seq))))))

(define (seq-fold fn acc seq)
  (let loop ((acc acc) (seq (to-seq seq)))
    (if (not (seq-empty? seq))
      (loop (fn acc (seq-first seq)) (seq-rest seq))
      acc)))

;; map ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-entry-key kvp)
  (call (com.github.krukow.clj_lang.MapEntry kvp) key))

(define (map-entry-val kvp)
  (call (com.github.krukow.clj_lang.MapEntry kvp) val))

(define (new-map . kvps)
  (let loop ((acc (call (com.github.krukow.clj_lang.PersistentHashMap '()) emptyMap))
             (kvps kvps))
    (if (null? kvps)
      acc
      (if (null? (cdr kvps))
	(loop (map-add acc (car kvps) '()) (cdr kvps))
	(loop (map-add acc (car kvps) (cadr kvps)) (cddr kvps))))))

(define (map-add x k v)
  (call (com.github.krukow.clj_lang.PersistentHashMap x)
        assoc
        (java.lang.Object k)
        (java.lang.Object v)))

(define (map-remove x k)
  (call (com.github.krukow.clj_lang.PersistentHashMap x)
        without
        (java.lang.Object k)))

(define (map-get x k)
  (call (com.github.krukow.clj_lang.PersistentHashMap x)
        valAt
        (java.lang.Object k)))

(define (map-count x)
  (call (com.github.krukow.clj_lang.PersistentHashMap x) count))

(define (map-contains-key? x k)
  (call (com.github.krukow.clj_lang.PersistentHashMap x)
        containsKey
        (java.lang.Object k)))

(define (map? x)
  (and (not (null? x))
       (eq? (get-type x)
            (typeof "com.github.krukow.clj_lang.PersistentHashMap"))))

(define (map-foreach fn map)
  (seq-foreach (lambda (kvp)
                 (fn (map-entry-key kvp) (map-entry-val kvp)))
               map))

(define (map-fold fn acc map)
  (seq-fold (lambda (acc kvp)
              (fn acc (map-entry-key kvp) (map-entry-val kvp)))
            acc
            map))

(define (map-keys x)
  (reverse
   (map-fold (lambda (acc k v) (cons k acc)) '() x)))

(define (map-merge target source)
  (map-fold (lambda (acc k v)
	      (map-add acc k v))
	    target
	    source))

(define (get-in-map m keys)
  (let loop ((m m) (keys keys))
    (if (null? keys)
      m
      (loop (map-get m (car keys)) (cdr keys)))))

;; vector ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-vector . xs)
  (let loop ((acc (call (com.github.krukow.clj_lang.PersistentVector '()) emptyVector))
             (i 0)
	     (xs xs))
    (if (null? xs)
      acc
      (loop (vector-set acc i (car xs)) (+ i 1) (cdr xs)))))

(define (vector-set vec index x)
  (call (com.github.krukow.clj_lang.PersistentVector vec) assocN (int index) (java.lang.Object x)))

(define (vector-count vec)
  (call (com.github.krukow.clj_lang.PersistentVector vec) count))

(define (vector-ref vec index)
  (call (com.github.krukow.clj_lang.PersistentVector vec) nth (int index)))

(define (vector-foreach fn vec)
  (seq-foreach fn vec))

(define (vector-fold fn acc vec)
  (seq-fold fn acc vec))

(define (vector->list vec)
  (reverse
   (vector-fold (lambda (acc x)
                  (cons x acc))
                vec)))

(define (vector? x)
  (and (not (null? x))
       (eq? (get-type x)
            (typeof "com.github.krukow.clj_lang.PersistentVector"))))

(define (list->vector lst)
  (apply new-vector lst))

;; set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-set . xs)
  (let loop ((acc (call (com.github.krukow.clj_lang.PersistentHashSet '()) emptySet))
             (xs xs))
    (if (null? xs)
      acc
      (loop (set-add acc (car xs)) (cdr xs)))))

(define (set-count set)
  (call (com.github.krukow.clj_lang.PersistentHashSet set) count))

(define (set-add set x)
  (call (com.github.krukow.clj_lang.PersistentHashSet set) cons (java.lang.Object x)))

(define (set-remove set x)
  (call (com.github.krukow.clj_lang.PersistentHashSet set) disjoin (java.lang.Object x)))

(define (set-contains set x)
  (call (com.github.krukow.clj_lang.PersistentHashSet set) contains (java.lang.Object x)))

(define (set-foreach set fn)
  (seq-foreach fn set))

(define (set-fold fn acc set)
  (seq-fold fn acc set))

(define (set->list set)
  (reverse
   (set-fold (lambda (acc x)
               (cons x acc))
             '()
             set)))

(define (set? x)
  (and (not (null? x))
       (eq? (get-type x)
            (typeof "com.github.krukow.clj_lang.PersistentHashSet"))))

(define (list->set lst)
  (apply new-set lst))

(define (set-union . sets)
  (let loop ((acc (new-set)) (sets sets))
    (if (null? sets)
      acc
      (loop (set-fold set-add (car sets) acc)
	    (cdr sets)))))

(define (set-except a b)
  (set-fold set-remove a b))

