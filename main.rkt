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
  (lambda (d)
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
    js))

;; Exactly like jsexpr? except allows procedure? as a value.
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
                                      'proc (create-new-method root v)))))]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; examples

;; ;; Google Plus
;; (define plus (local-discovery-document->service "plus.js"))
;; (defproc plus people search)
;; (people-search (hasheq 'query "Greg Hendershott"
;;                      'key (api-key)))


;; URL Shortener (goo.gl)
(require rackunit)
(define goo.gl (local-discovery-document->service "urlshortener.js"))
(defproc goo.gl url insert)
(defproc goo.gl url get)
(define orig-url "http://www.racket-lang.org/")
(define shrink (url-insert (hasheq 'body (hasheq 'longUrl orig-url)
                                   'key (api-key))))
(define short-url (dict-ref shrink 'id))
(define expand (url-get (hasheq 'shortUrl short-url
                                'key (api-key))))
(define long-url (dict-ref expand 'longUrl))
(check-equal? orig-url long-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code generation

;; Although generating code files seems extremely un-Rackety, I'm not
;; sure this is really possible to do with macros. Plus, the
;; code-generation approach at least gives library users some actual
;; code to look at, including documentation in the form of
;; comments. So...???

(define/contract (discovery-document->racket-code root)
  (jsexpr? . -> . any)
  (define (do j)
    (for ([(k v) (in-hash j)])
      (match k
        ['resources (newline)
                    (displayln (make-string 78 #\;))
                    (for ([(rn rv) v])
                      (printf ";;; RESOURCE: ~a\n" rn)
                      (for ([(k v) (in-hash rv)])
                        (match k
                          ['methods (for ([(mn mv) (in-hash v)])
                                      (do-method mn mv))]
                          ['resources (do v)] ;sub-resources
                          [else (cond [(string? v) (printf "~a: ~a\n" k v)]
                                      [else (displayln k)])])))]
        ['parameters (newline)
                     (displayln (make-string 78 #\;))
                     (displayln ";;; API PARAMETERS")
                     (displayln ";; Parameters for all API functions.")
                     (for ([(k v) v])
                       (printf ";; `~a'\n" k)
                       (for ([(k v) v])
                         (printf ";;   ~a: ~a\n" k v)))
                     (newline)]
        [else (printf ";; ~a: ~a\n"
                      k
                      (if (string? v) v ""))])))
  (define (do-method mn mv)
    (define name (string->symbol (hash-ref mv 'id)))
    (define api-params (hash-keys (hash-ref root 'parameters)))
    (define req-params (map string->symbol (hash-ref mv 'parameterOrder '())))
    (define opt-params (remove* req-params (hash-keys (hash-ref mv 'parameters (hash)))))
    (define body-params
      (hash-ref (hash-ref (hash-ref root 'schemas)
                          (string->symbol
                           (hash-ref (hash-ref mv 'request (hash)) '$ref ""))
                          (hash))
                'properties
                (hash)))
    (define body-param-names (hash-keys body-params))
    (define all-opt-params (append opt-params body-param-names api-params))
    (newline)
    (displayln (make-string 78 #\;))
    (printf ";; FUNCTION: ~a\n" name)
    (printf ";; PARAMETERS:\n")
    (printf ";; (See above for parameters that apply to all API methods.)\n")
    (printf ";;\n")
    (for ([(k v) (hash-ref mv 'parameters (hash))])
      (printf ";; `~a'\n" k)
      (for ([(k v) v])
        (printf ";;   ~a: ~a\n" k v)))
    (for ([(k v) body-params])
      (printf ";; `~a'\n" k)
      (for ([(k v) v])
        (printf ";;   ~a: ~a\n" k v)))
    (define qps (append req-params opt-params))
    (pretty-print
     `(define (,name
               ,@req-params
               ,@(letrec ([flatter (lambda (xs)
                                     (match xs
                                       [(list (list a b) more ...)
                                        (cons a (cons b (flatter more)))]
                                       [(list) (list)]))])
                   (flatter (map (lambda (x)
                                   (list (string->keyword (symbol->string x))
                                         (list x ''NONE)))
                                 all-opt-params)))
               )
        (define base-uri ,(hash-ref root 'baseUrl))
        (define res-path ,(hash-ref mv 'path))
        (define _qpstr (alist->form-urlencoded
                        (filter-map
                         (lambda (k v)
                           (cond [(eq? v 'NONE) #f]
                                 [else (cons (string->symbol k) v)]))
                         (list ,@(map symbol->string qps))
                         (list ,@qps))))
        (define qpstr (cond [(equal? _qpstr "") ""]
                            [else (string-append "?" _qpstr)]))
        (define url (string->url (string-append base-uri res-path qpstr)))
        (define h (list "Content-Type: application/json"))
        (define body (jsexpr->bytes
                      (for/hasheq ([k (list ,@(map symbol->string body-param-names))]
                                   [v (list ,@body-param-names)]
                                   #:when (not (eq? v 'NONE)))
                        (values (string->symbol k) v)))) 
        (define in
          ,(match (hash-ref mv 'httpMethod)
             ["GET" `(get-pure-port url h)]
             ["POST" `(post-pure-port url body h)]
             [else `(error ',name "TO-DO")]))
        (define js (bytes->jsexpr (port->bytes in)))
        (close-input-port in)
        js))
    (newline))
  (do root))

;; (discovery-document->racket-code (load-discovery-document "urlshortener.js"))
;; (discovery-document->racket-code (load-discovery-document "plus.js"))
