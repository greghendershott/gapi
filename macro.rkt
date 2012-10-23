#lang racket

(require (for-syntax racket/path
                     racket/match
                     racket/list
                     json
                     "main.rkt")
         scribble/base
         (except-in net/url url) ;scribble/base provides `url'
         json
         net/uri-codec
         "main.rkt")

(provide require-gapi-doc
         api-key
         (all-from-out "main.rkt"))

(begin-for-syntax

 (define (gen-racket js stx)
   #`(begin
       #,@(append*
           (for/list ([(k v) (hash-ref js 'resources)])
             (for/list ([(k v) (hash-ref v 'methods)])
               (do-method stx js v))))))
 
  (define (do-method stx root meth)
    (define name (string->symbol
                  (regexp-replace* #rx"\\." (hash-ref meth 'id) "-")))
    (define api-param-names (hash-keys (hash-ref root 'parameters)))
    (define params (hash-ref meth 'parameters (hash)))
    (define (required? x)
      (and (hash-has-key? x 'required)
           (hash-ref x 'required)))
    (define req-params (for/hasheq ([(k v) params]
                                    #:when (required? v))
                         (values k v)))
    (define opt-params (for/hasheq ([(k v) params]
                                    #:when (not (required? v)))
                         (values k v)))
    (define req-param-names (hash-keys req-params))
    (define opt-param-names (hash-keys opt-params))
    (define _body-params
      (hash-ref (hash-ref (hash-ref root 'schemas)
                          (string->symbol
                           (hash-ref (hash-ref meth 'request (hash)) '$ref ""))
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
    (define body-param-names (hash-keys body-params))
    (define all-opt-param-names (append opt-param-names
                                        body-param-names
                                        api-param-names))
    (define qps (append req-param-names opt-param-names api-param-names))
    (define symbol->keyword (compose1 string->keyword symbol->string))
    (define base-uri (hash-ref root 'baseUrl))
    (define res-path (hash-ref meth 'path))
    #`(define (#,(datum->syntax stx name)
               [headers '()]
               #,@(append* (map (lambda (x)
                                  (list (symbol->keyword x)
                                        x))
                                req-param-names))
               #,@(append* (map (lambda (x)
                                  (list (symbol->keyword x)
                                        (list x
                                              (cond [(eq? x 'key)
                                                     #'(api-key)]
                                                    [else ''N/A]))))
                                all-opt-param-names)))
        (define _qpstr (alist->form-urlencoded
                        (filter-map
                         (lambda (k v)
                           (cond [(eq? v 'N/A) #f]
                                 [else (cons (string->symbol k) v)]))
                         (list #,@(map symbol->string qps))
                         (list #,@qps))))
        (define qpstr (cond [(equal? _qpstr "") ""]
                            [else (string-append "?" _qpstr)]))
        (define url (string->url (string-append #,base-uri #,res-path qpstr)))
        (define h (cons "Content-Type: application/json" headers))
        (define body
          (jsexpr->bytes
           (for/hasheq ([k (list #,@(map symbol->string body-param-names))]
                        [v (list #,@body-param-names)]
                        #:when (not (eq? v 'N/A)))
             (values (string->symbol k) v)))) 
        (define in
          #,(match (hash-ref meth 'httpMethod)
              ["GET" #'(get-pure-port url h)]
              ["POST" #'(post-pure-port url body h)]
              ["PUT" #'(put-pure-port url body h)]
              [else #'(error ',name "TO-DO")]))
        (define js (bytes->jsexpr (port->bytes in)))
        (close-input-port in)
        js))

  (define (gen gen)
    (define (m stx)
      (syntax-case stx ()
        [(_ js-file)
         (or (string? (syntax-e #'js-file))
             (symbol? (syntax-e #'js-file)))
         (gen (load-discovery-document (syntax-e #'js-file)) stx)]))
    m))

(define-syntax require-gapi-doc (gen gen-racket))
