#lang racket

(require "dynamic.rkt"
         (for-syntax racket/match
                     racket/list
                     json
                     "dynamic.rkt"))

(provide require-gapi-doc
         (all-from-out "dynamic.rkt"))

(begin-for-syntax
 (define (method-specs root)
   ;; (jsexpr? . -> . (listof method-spec?))
   ;; Note that resources can be arbitrarily deep,
   ;; e.g. resource/resource/method.
   (define (do j xs)
     (for/list ([(k v) (in-hash j)])
       (match k
         ['resources (append xs (for/list ([x (hash-values v)])
                                  (do x xs)))]
         ['methods (append xs (for/list ([x (hash-values v)])
                                (create-method-spec root x)))]
         [else xs])))
   (flatten (do root '()))))

(define-syntax (require-gapi-doc stx)
  (syntax-case stx ()
    [(_ file)
     (or (string? (syntax-e #'file))
         (symbol? (syntax-e #'file)))
     (let* ([dd (load-discovery-document (syntax-e #'file))]
            [specs (method-specs dd)])
       #`(begin
           #,@(for/list ([spec specs])
                (with-syntax ([id (datum->syntax stx (method-spec-id spec))]
                              [ms (datum->syntax stx spec)])
                  #'(define id (method-spec->procedure ms))))))]))
