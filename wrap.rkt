#lang racket

;; Just things needed by both macro.rkt and dynamic.rkt, to create a
;; function that wraps an API method.

(require json
         net/url
         net/uri-codec
         "doc.rkt"
         "patch-port.rkt"
         "dict-merge.rkt")

(provide create-method-spec
         method-spec->procedure
         (struct-out method-spec))

;; History: Originally, `create-method' was a single function, used
;; only by `discovery-document->service which loads disc docs
;; "dynamically" at run-time.  The guts of that function were very
;; nearly duplicated in the template of a macro. This duplicated code
;; was undesirable and I wanted to foctor it out to be shared.
;;
;; As a result, I split `create-method' into two new functions:
;;
;; 1. `create-method-spec' takes a discovery document and extracts the
;; bare minimum necessary to create a wrapper function, returning that
;; in the `method-spec' struct.
;;
;; 2. `method-spec->procedure' uses the struct to creates the wrapper
;; function.
;;
;; When using this library "dynamically" to load a disc doc at
;; runtime, the new `create-method' can be used; its simply a
;; composition of its former two pieces. For that scenario, this
;; reorganziation has no value.
;;
;; But when using this library in the other appraoch, where the disc
;; docs are loaded at compile-time with `require-gapi-doc', it
;; matters.  In that scenario, the macro transformer uses 1 at compile
;; time to create a compile-time method-spec struct. The call to 2
;; must be at run-time, and the method-spec struct as a literal value
;; in the generated syntax to call 2. In other words,
;; method-spec->procedure still works at run-time. It has to be done
;; this way because the struct can't exist in both syntax and run-time
;; phases, as a Racket varaible. However provided the struct is
;; #:prefab, it can be quoted as a literal value in the syntax.

(struct method-spec (id
                     base-uri
                     path
                     api-params
                     req-params
                     opt-params
                     query-params
                     body-params
                     http-method)
        #:prefab) ;lets us quote compile-time struct as a literal
                  ;value in macro template

(define/contract (create-method-spec dd method)
  (jsexpr? jsexpr? . -> . method-spec?)
  (define id (string->symbol (regexp-replace* #rx"\\."
                                              (hash-ref method 'id)
                                              "-")))
  (define base-uri (string-append (hash-ref dd 'rootUrl)
                                  (hash-ref dd 'servicePath)))
  (define path (hash-ref method 'path))
  (define api-params (hash-ref dd 'parameters))
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
  (define query-params (for/list ([(k v) (dict-merge api-params
                                                     method-params)]
                                  #:when (equal? "query"
                                                 (hash-ref v 'location)))
                         k))
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
                 #:when (not (or (hash-has-key? req-params k)
                                 (hash-has-key? opt-params k)
                                 (hash-has-key? api-params k))))
      (values k v)))
  (define http-method (hash-ref method 'httpMethod))
  (method-spec id
               base-uri
               path
               (hash-keys api-params)
               (hash-keys req-params)
               (hash-keys opt-params)
               query-params
               (hash-keys body-params)
               http-method))

(define/contract (method-spec->procedure ms)
  (method-spec? . -> . procedure?)
  (match-define (method-spec
                 id
                 base-uri
                 path
                 api-params
                 req-params
                 opt-params
                 query-params
                 body-params
                 http-method) ms)
  ;; A procedure that takes a dict, and does the actual work of making
  ;; a request to the server.
  (define/contract (f/dict d)
    (dict? . -> . jsexpr?)
    (define u (string-append base-uri
                             (template-path path d)))
    (define qps (for/list ([(k v) (in-dict d)]
                           #:when (member k query-params))
                  (cons k v)))
    (define url
      (string->url
       (cond [(empty? qps) u]
             [else (string-append u
                                  "?"
                                  (alist->form-urlencoded qps))])))
    (define body
      (jsexpr->bytes
       (for/hasheq ([k body-params])
         (values k (dict-ref d k "")))))
    (define h (list "Content-Type: application/json"))
    (define in
      (match http-method
        ["GET" (get-pure-port url h)]
        ["POST" (post-pure-port url body h)]
        ["PUT" (put-pure-port url body h)]
        ["DELETE" (delete-pure-port url h)]
        ["PATCH" (patch-pure-port url body h)]
        [(var m) (error 'create-method "HTTP method ~a not supported" m)]))
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
  ;; optional keyword arguments.
  (define req-kws (sort (map symbol->keyword req-params) keyword<=?))
  (define opt-kws (map symbol->keyword opt-params))
  (define body-kws (map symbol->keyword body-params))
  (define api-kws (map symbol->keyword api-params))
  (define all-kws (sort (append req-kws opt-kws body-kws api-kws)
                        keyword<=?))
  (procedure-reduce-keyword-arity f/kw 0 req-kws all-kws))

(define (template-path str d)
  (string-join
   (for/list ([x (regexp-split #rx"/" str)])
     (match x
       [(pregexp "^\\{(.+)\\}$" (list _ k)) (dict-ref d (string->symbol k))]
       [else x]))
   "/"))
