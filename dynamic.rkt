#lang racket

(require json
         net/url
         net/uri-codec
         "main.rkt"
         "dict-merge.rkt"
         )

(provide discovery-document->service
         local-discovery-document->service
         online-discovery-document->service
         )

(define/contract (create-new-method dd method)
  (jsexpr? jsexpr? . -> . (dict? . -> . jsexpr?))
  (define base-uri (string-append (hash-ref dd 'rootUrl)
                                  (hash-ref dd 'servicePath)))
  ;; Parameters are the union of those for entire API and those for
  ;; this method:
  (define params (dict-merge (hash-ref dd 'parameters (hash))
                             (hash-ref method 'parameters (hash))))
  (define request
    (match (hash-ref method 'httpMethod)
      ["GET" (lambda (url body h) (get-pure-port url h))]
      ["POST" post-pure-port]
      ["PUT" put-pure-port]))
  ;; Make a request to the server
  (define (inner d)
    (define body (jsexpr->bytes (dict-ref d 'body (hasheq))))
    (define u (string-append base-uri
                             (template-path (hash-ref method 'path) d)))
    (define non-path (filter values
                             (for/list ([(k v) d])
                               (define p (hash-ref params k #f))
                               (cond
                                [(not p) #f]
                                [else (match (hash-ref p 'location)
                                        ["path" #f]
                                        [else (cons k v)])]))))
    (define url
      (string->url
       (cond [(empty? non-path) u]
             [else (string-append u
                                  "?"
                                  (alist->form-urlencoded non-path))])))
    (define h (list "Content-Type: application/json"))
    (define in (request url body h))
    (define js (bytes->jsexpr (port->bytes in)))
    (close-input-port in)
    js)
  ;; Handle nextPageToken and multiple "pages" of results by making
  ;; multiple requests.
  (define (outer d)
    (define js (inner d))
    (define page-token (hash-ref js 'nextPageToken #f))
    (cond [(and page-token
                (hash-has-key? js 'items))
           (hash-update js
                        'items
                        (lambda (xs)
                          (append xs
                                  (hash-ref (outer (dict-set* d
                                                              'pageToken
                                                              page-token))
                                            'items))))]
          [else js]))
  outer
  )

(define (template-path str d)
  (string-join (for/list ([x (regexp-split #rx"/" str)])
                 (match x
                   [(pregexp "^\\{(.+)\\}$" (list _ k)) (dict-ref d k)]
                   [else x]))
               "/"))

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
                                      'proc (create-new-method root v)))))]
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
  (define shrink (url-insert (hasheq 'body (hasheq 'longUrl orig-url)
                                     'key (api-key))))
  (define short-url (dict-ref shrink 'id))
  (define expand (url-get (hasheq 'shortUrl short-url
                                  'key (api-key))))
  (define long-url (dict-ref expand 'longUrl))
  (check-equal? orig-url long-url)
  )
