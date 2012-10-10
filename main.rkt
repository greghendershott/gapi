#lang racket

(require json
         net/url
         net/uri-codec
         "dict-merge.rkt"
         )

(define (discovery-url name)
  (string->url
   (format "https://www.googleapis.com/discovery/v1/apis/~a/v1/rest"
           name)))

(define (download-discovery-document name [path (string-append name ".js")])
  (call-with-output-file* path
    (lambda (out)
      (call/input-url (discovery-url name)
                      get-pure-port
                      (lambda (in)
                        (copy-port in out))))
    #:mode 'text
    #:exists 'replace))

(define/contract (load-discovery-document path)
  (path-string? . -> . jsexpr?)
  (bytes->jsexpr (file->bytes path)))

(define/contract (get-discovery-document name)
  (string? . -> . jsexpr?)
  (call/input-url (discovery-url name)
                  get-pure-port
                  (compose1 bytes->jsexpr port->bytes)))

(define (template-path str d)
  (string-join (for/list ([x (regexp-split #rx"/" str)])
                 (match x
                   [(pregexp "^\\{(.+)\\}$" (list _ k)) (dict-ref d k)]
                   [else x]))
               "/"))

(define (dict-refs d . ks)
  (for/fold ([d d]) ([k ks])
    (dict-ref d k)))

(define/contract (create-new-method dd name method)
  (jsexpr? symbol? hash? . -> . (dict? . -> . jsexpr?))
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
  (lambda (d)
    (define body (dict-ref d 'body #""))
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
    js))

;; Exactly ike jsexpr? except allows procedure? as a value.
(define (service? x #:null [jsnull (json-null)])
  (let loop ([x x])
    (or (procedure? x) ; <=== ONLY difference from jsexpr?
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
                                      'proc (create-new-method root k v)))))]
        [else (values k v)])))
  (do root))

(define local-discovery-document->service
  (compose1 discovery-document->service load-discovery-document))
(define online-discovery-document->service
  (compose1 discovery-document->service get-discovery-document))

(define-syntax (defproc stx)
  (syntax-case stx ()
    [(_ service resource method)
     (let* ([assert-id (lambda (id)
                         (unless (identifier? id)
                                 (raise-syntax-error 
                                  #f
                                  "expected an identifier"
                                  stx
                                  id)))]
            [hyphenate-ids (lambda (a b)
                             (assert-id a)
                             (assert-id b)
                             (datum->syntax
                              a
                              (string->symbol (format "~a-~a"
                                                      (syntax->datum a)
                                                      (syntax->datum b)))
                              a a a))])
       (with-syntax ([id (hyphenate-ids #'resource #'method)])
         #'(begin (define id (method-proc service
                                          (syntax->datum #'resource)
                                          (syntax->datum #'method)))
                  (provide id))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some accessors for the discovery document jsexpr.
;;
;; The case-lambdas provide a shortcut for resources that aren't nested.

(define (api-parameters s)
  (dict-ref s 'parameters))

(define (resources s)
  (dict-ref s 'resources '()))
(define (resource-names s)
  (dict-keys (resources s)))
(define (resource s rn)
  (dict-ref (resources s) rn '()))
(define methods
  (case-lambda
    [(r) (dict-ref r 'methods '())]
    [(s rn) (methods (resource s rn))]))
(define method-names
  (case-lambda
    [(r) (dict-keys (methods r))]
    [(s rn) (method-names (resource s rn))]))
(define method
  (case-lambda
    [(r mn) (dict-ref (methods r) mn '())]
    [(s rn mn) (method (resource s rn) mn)]))
(define method-proc
  (case-lambda
    [(r mn) (dict-ref (method r mn) 'proc '())]
    [(s rn mn) (method-proc (resource s rn) mn)]))
(define method-parameters
  (case-lambda
    [(r mn) (dict-ref (method r mn) 'parameters '())]
    [(s rn mn) (method-parameters (resource s rn) mn)]))

(define (schemas s)
  (dict-ref s 'schemas '()))
(define (schema-names s)
  (dict-keys (schemas s)))
(define (schema s sn)
  (dict-ref (schemas s) sn '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-api-key [file (build-path (find-system-path 'home-dir)
                                        ".google-api-key")])
  (match (file->string file #:mode 'text)
    [(regexp "^\\s*(.*?)\\s*(?:[\r\n]*)$" (list _ k)) k]
    [else (error 'read-api-key "Bad format for ~a" file)]))
(define api-key (make-parameter (read-api-key)))

;; (define service (build-service "urlshortener.js"))
;; ((dict-refs service 'url 'insert) (hash 'body #"{\"longUrl\": \"http://www.google.com\"}"))
;; ((dict-refs service 'url 'get) (hash 'shortUrl "http://goo.gl/fbsS"
;;                                      'projections "FULL"))

(define plus (local-discovery-document->service "plus.js"))

(defproc plus people search)
(people-search (hash 'query "Greg Hendershott"
                     'key (api-key)))



;; ((dict-refs service 'people 'search) (hash 'query "Greg Hendershott"
;;                                            'key (api-key)))

;;(defprocs (list (list service people search)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

