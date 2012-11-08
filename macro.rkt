#lang racket

(require (for-syntax racket/path
                     racket/match
                     racket/list
                     json
                     "dynamic.rkt")
         net/url
         json
         net/uri-codec
         "dynamic.rkt")

(provide require-gapi-doc
         api-key
         (all-from-out "dynamic.rkt"))

(begin-for-syntax
 (define (method-specs root)
   ;; (jsexpr? . -> . (listof method-spec?))
   (define specs (make-hasheq)) ;FIXME: using a mutable list, really
   (define (do j)
     (for ([(k v) (in-hash j)])
       (match k
         ['resources
          (for ([(k v) (in-hash v)])
            (do v))]
         ['methods
          (for ([(k v) (in-hash v)])
            (hash-set! specs
                       (create-method-spec root v)
                       0))]
         [else (void)])))
   (do root)
   (hash-keys specs)))

(define-syntax (require-gapi-doc stx)
  (syntax-case stx ()
    [(_ file)
     (or (string? (syntax-e #'file))
         (symbol? (syntax-e #'file)))
     (let* ([dd (load-discovery-document (syntax-e #'file))]
            [specs (method-specs dd)])
       #`(begin
           #,@(for/list ([spec specs])
                (with-syntax ([name (datum->syntax stx (method-spec-id spec))]
                              [ms (datum->syntax stx spec)])
                  #'(define name (method-spec->procedure ms))))))]))
