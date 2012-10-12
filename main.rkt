#lang racket

(require json
         net/url
         net/uri-codec)

(provide discovery-url
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
;;
;; Examples

#|

;; How many preferred (e.g. latest version, not deprecated)  services?
(length (hash-ref (list-services #:only-preferred? #t) 'items))
;; All?
(length (hash-ref (list-services #:only-preferred? #f) 'items))

;; Show all the preferred APIs
(map (lambda (x)
       (list (hash-ref x 'name)
             (hash-ref x 'version)
             (hash-ref x 'description)))
     (hash-ref (list-services) 'items))

;; Download all to discovery documents in "services" subdir
(define (download-all)
  (for ([x (hash-ref (list-services #:only-preferred? #t) 'items)])
    (define name (hash-ref x 'name))
    (define ver (hash-ref x 'version))
    (define fname (string-append name "." ver ".js"))
    (define path (build-path 'same "services" fname))
    (printf "Downloading ~s to ~s.\n" name (path->string path))
    (flush-output)
    (download-discovery-document name path)))
(download-all)

|#


