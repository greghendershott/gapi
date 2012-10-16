#lang racket

(require json
         net/url
         net/uri-codec)

(provide list-services
         download-discovery-document
         load-discovery-document
         get-discovery-document
         api-parameters
         resources
         resource-names
         resource
         methods
         method-names
         method
         method-proc
         method-parameters
         schemas
         schema-names
         schema
         api-key
         paged
         )

(define (list-services #:name [name 'N/A]
                       #:label [label 'N/A]
                       #:only-preferred? [preferred #t])
  (define base "https://www.googleapis.com/discovery/v1/apis")
  (define qps (alist->form-urlencoded
               (filter-map (lambda (k v)
                             (cond [(eq? v 'N/A) #f]
                                   [(eq? v #t) (cons k "true")]
                                   [(eq? v #f) (cons k "false")]
                                   [else (cons k v)]))
                           (list 'name 'label 'preferred)
                           (list name label preferred))))
  (define u (string-append base (if (equal? qps "") "" "?") qps))
  (call/input-url (string->url u)
                  get-pure-port
                  (compose1 bytes->jsexpr port->bytes)))

(define (discovery-url name ver)
  (string->url
   (format "https://www.googleapis.com/discovery/v1/apis/~a/~a/rest"
           name ver)))

(define (download-discovery-document name ver [path (string-append name ".js")])
  (call-with-output-file* path
    (lambda (out)
      (call/input-url (discovery-url name ver)
                      get-pure-port
                      (lambda (in)
                        (copy-port in out))))
    #:mode 'text
    #:exists 'replace))

(define/contract (load-discovery-document path)
  (path-string? . -> . jsexpr?)
  (bytes->jsexpr (file->bytes path)))

(define/contract (get-discovery-document name ver)
  (string? string? . -> . jsexpr?)
  (call/input-url (discovery-url name)
                  get-pure-port
                  (compose1 bytes->jsexpr port->bytes)))

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

;; Read an API key from a dot file, defaulting to ~/.google-api-key
(define (read-api-key [file (build-path (find-system-path 'home-dir)
                                        ".google-api-key")])
  (match (file->string file #:mode 'text)
    [(regexp "^\\s*(.*?)\\s*(?:[\r\n]*)$" (list _ k)) k]
    [else (error 'read-api-key "Bad format for ~a" file)]))
(define api-key (make-parameter (read-api-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convenience when you don't want to process results in "pages", but
;; rather want one -- albeit potentially huge -- list of items.
(define-syntax-rule (paged (func args ...))
  (let loop ([js (func args ...)])
    (define page-token (hash-ref js 'nextPageToken #f))
    (cond [(and page-token
                (hash-has-key? js 'items))
           (hash-update js
                        'items
                        (lambda (xs)
                          (append xs
                                  (hash-ref (loop (func args ... 
                                                        #:pageToken
                                                        page-token))
                                            'items))))]
          [else js])))
