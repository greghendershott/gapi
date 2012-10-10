#lang racket

(provide dict-merge)

(define/contract (dict-merge . ds)
  (() #:rest (listof dict?) . ->* . dict?)
  (match ds
    ;; If first is dict-can-functional-set? reuse it as the base
    ;; dict, to save work.
    [(list (? dict-can-functional-set? d0) ds ...)
     (for/fold ([d0 d0])
               ([d (in-list ds)])
       (for/fold ([d0 d0])
                 ([(k v) (in-dict d)])
         (dict-set d0 k v)))]
    ;; If first isn't dict-can-functional-set?, make an empty hash to
    ;; be the base dict.
    [(list d0 ds ...)
     (for/fold ([d0 (hash)])
               ([d (in-list (cons d0 ds))])
       (for/fold ([d0 d0])
                 ([(k v) (in-dict d)])
         (dict-set d0 k v)))]     
    [(list d) d]
    [(list) (hash)]))

(module+ test
  (require rackunit)
  (check-equal? (dict-merge (vector 0 1)
                            (hash 2 2 3 3)
                            '([4 . 4] [5 . 5] [6 . 6]))
                (hash 0 0 1 1 2 2 3 3 4 4 5 5 6 6))
  (check-equal? (dict-merge (hash 1 1 2 2 3 3 ))
                (hash 1 1 2 2 3 3))
  (check-equal? (dict-merge (hash))
                (hash))
  )
