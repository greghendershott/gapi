#lang racket

(require json
         net/url
         net/uri-codec
         "doc.rkt"
         "wrap.rkt")

(provide service?
         discovery-document->service
         local-discovery-document->service
         online-discovery-document->service
         (all-from-out "doc.rkt"))

(define (create-method dd method)
  (method-spec->procedure (create-method-spec dd method)))

;; Exactly like jsexpr? except allows procedure? as a value.
(define (service? x #:null [jsnull (json-null)])
  (let loop ([x x])
    (or (procedure? x) ; <=== ONLY difference from `jsexpr?'
        (exact-integer? x)
        (inexact-real? x)
        (boolean? x)
        (string? x)
        (eq? x jsnull)
        (and (list? x) (andmap loop x))
        (and (hash? x) (for/and ([(k v) (in-hash x)])
                         (and (symbol? k) (loop v)))))))

(define/contract (discovery-document->service root)
  (jsexpr? . -> . service?)
  (define (do j)
    (for/hasheq ([(k v) (in-hash j)])
      (match k
        ['resources
         (values 'resources
                 (for/hasheq ([(k v) (in-hash v)])
                   (values k (do v))))]
        ['methods
         (values 'methods
                 (for/hasheq ([(k v) (in-hash v)])
                   (values k
                           (hash-set* v
                                      'proc (create-method root v)))))]
        [else (values k v)])))
  (do root))

(define local-discovery-document->service
  (compose1 discovery-document->service load-discovery-document))
(define online-discovery-document->service
  (compose1 discovery-document->service get-discovery-document))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Examples

;; ;; Google Plus
;; (define plus (local-discovery-document->service "vendor/plus.v1.js"))
;; (define people-search (method-proc plus 'people 'search))
;; (define js (people-search (hasheq 'query "Greg Henderson"
;;                                   'key (api-key))))
;; (displayln (length (hash-ref js 'items '())))
             
;; URL Shortener (goo.gl)
(module+ test
  (require rackunit)
  (define goo.gl (local-discovery-document->service
                  "vendor/urlshortener.v1.js"))
  (define url-insert (method-proc goo.gl 'url 'insert))
  (define url-get (method-proc goo.gl 'url 'get))
  (define orig-url "http://www.racket-lang.org/")
  (define shrink (url-insert #:longUrl orig-url
                             #:key (api-key)))
  (define short-url (dict-ref shrink 'id))
  (define expand (url-get #:shortUrl short-url
                          #:key (api-key)))
  (define long-url (dict-ref expand 'longUrl))
  (check-equal? orig-url long-url)
  )
