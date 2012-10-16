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
         (all-from-out "main.rkt")
         )

(define/contract (create-new-method dd method)
  (jsexpr? jsexpr? . -> . procedure?)
  (define base-uri (string-append (hash-ref dd 'rootUrl)
                                  (hash-ref dd 'servicePath)))
  ;; Parameters are the union of those for entire API and those for
  ;; this method:
  (define params (dict-merge (hash-ref dd 'parameters (hash))
                             (hash-ref method 'parameters (hash))))
  (define api-params (hash-ref dd 'parameters (hasheq)))
  (define method-params (hash-ref method 'parameters (hasheq)))
  (define (required? x)
    (and (hash-has-key? x 'required)
         (hash-ref x 'required)))
  (define req-params (for/hasheq ([(k v) method-params]
                                  #:when (required? v))
                       (values k v)))
  (define opt-params (for/hasheq ([(k v) method-params]
                                  #:when (not (required? v)))
                       (values k v)))
  (define _body-params
    (hash-ref (hash-ref (hash-ref dd 'schemas)
                        (string->symbol
                         (hash-ref (hash-ref method 'request (hash)) '$ref ""))
                        (hash))
              'properties
              (hash)))
  ;; The "licensing" API has a problem where it duplicates a
  ;; required parameter (such as "userID" or "productID") in the
  ;; body parameters. Filter such problems here.
  (define body-params
    (for/hasheq ([(k v) _body-params]
                 #:when (not (hash-has-key? req-params k)))
      (values k v)))
  (define request
    (match (hash-ref method 'httpMethod)
      ["GET" (lambda (url body h) (get-pure-port url h))]
      ["POST" post-pure-port]
      ["PUT" put-pure-port]))
  ;; A procedure that takes a dict, and does the actual work of making
  ;; a request to the server.
  (define/contract (f/dict d)
    (dict? . -> . jsexpr?)
    (define u (string-append base-uri
                             (template-path (hash-ref method 'path) d)))
    (define qps (filter values
                        (for/list ([(k v) (in-dict d)])
                          (define p (hash-ref params k #f))
                          (cond
                           [(not p) #f]
                           [else (match (hash-ref p 'location)
                                   ["path" #f]
                                   [else (cons k v)])]))))
    (define url
      (string->url
       (cond [(empty? qps) u]
             [else (string-append u
                                  "?"
                                  (alist->form-urlencoded qps))])))
    (define body
      (jsexpr->bytes
       (for/hasheq ([k (hash-keys body-params)])
         (values k (dict-ref d k "")))))
    (define h (list "Content-Type: application/json"))
    (define in (request url body h))
    (define js (bytes->jsexpr (port->bytes in)))
    (close-input-port in)
    js)
  ;; Wrap f/dict in a procedure that takes keyword arguments instead
  ;; of a dict.
  (define symbol->keyword (compose1 string->keyword symbol->string))
  (define keyword->symbol (compose1 string->symbol keyword->string))
  (define (keyword<=? a b) (string<=? (keyword->string a) (keyword->string b)))
  (define (maybe-add-api-key d)
    (cond [(dict-has-key? d 'key) d]
          [else (dict-set* d 'key (api-key))]))
  (define f/kw (make-keyword-procedure
                (lambda (kws vs . rest)
                  (f/dict (maybe-add-api-key (map cons
                                                  (map keyword->symbol kws)
                                                  vs))))))
  ;; Tweak that procedure to accept only specific required and
  ;; optional keyword argumetns.
  (define req-kws (map symbol->keyword (hash-keys req-params)))
  (define opt-kws (map symbol->keyword (hash-keys opt-params)))
  (define body-kws (map symbol->keyword (hash-keys body-params)))
  (define api-kws (map symbol->keyword (hash-keys api-params)))
  (define all-kws (sort (append req-kws opt-kws body-kws api-kws) keyword<=?))
  (procedure-reduce-keyword-arity f/kw 0 req-kws all-kws))

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
  (define shrink (url-insert #:longUrl orig-url
                             #:key (api-key)))
  (define short-url (dict-ref shrink 'id))
  (define expand (url-get #:shortUrl short-url
                          #:key (api-key)))
  (define long-url (dict-ref expand 'longUrl))
  (check-equal? orig-url long-url)
  )
