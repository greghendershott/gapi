#lang at-exp racket

(provide gapi-doc->racket-code
         gapi-doc->scribble-code
         require/gapi-doc
         api-key)

(require (for-syntax racket/path racket/match racket/list json)
         scribble/base
         "main.rkt") ;for api-key

(begin-for-syntax

 (define (gen-racket js stx)
   #`(begin
       (require json net/url net/uri-codec)
       #,@(car
           (for/list ([(k v) (hash-ref js 'resources)])
             (for/list ([(k v) (hash-ref v 'methods)])
               (do-method stx js k v))))))
 
  (define (do-method stx root mn mv)
    (define name (string->symbol (hash-ref mv 'id)))
    (define api-param-names (hash-keys (hash-ref root 'parameters)))
    (define params (hash-ref mv 'parameters (hash)))
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
                           (hash-ref (hash-ref mv 'request (hash)) '$ref ""))
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
    #`(define (#,(datum->syntax stx name)
               #,@req-param-names
               #,@(append* (map (lambda (x)
                                  (list (string->keyword (symbol->string x))
                                        (list x ''NONE)))
                                all-opt-param-names)))
        (define base-uri #,(hash-ref root 'baseUrl))
        (define res-path #,(hash-ref mv 'path))
        (define _qpstr (alist->form-urlencoded
                        (filter-map
                         (lambda (k v)
                           (cond [(eq? v 'NONE) #f]
                                 [else (cons (string->symbol k) v)]))
                         (list #,@(map symbol->string qps))
                         (list #,@qps))))
        (define qpstr (cond [(equal? _qpstr "") ""]
                            [else (string-append "?" _qpstr)]))
        (define url (string->url (string-append base-uri res-path qpstr)))
        (define h (list "Content-Type: application/json"))
        (define body
          (jsexpr->bytes
           (for/hasheq ([k (list #,@(map symbol->string body-param-names))]
                        [v (list #,@body-param-names)]
                        #:when (not (eq? v 'NONE)))
             (values (string->symbol k) v)))) 
        (define in
          #,(match (hash-ref mv 'httpMethod)
              ["GET" #'(get-pure-port url h)]
              ["POST" #'(post-pure-port url body h)]
              ["PUT" #'(put-pure-port url body h)]
              [else #'(error ',name "TO-DO")]))
        (define js (bytes->jsexpr (port->bytes in)))
        (close-input-port in)
        js))

  (define (gen-scribble js stx)
    #`@list{1
            @section{Raw spec}
            @list{The raw spec from
                  @(tt #,(path->string (syntax-source stx)))
                  is:
                  @verbatim{
                    @#,(format "~s" js)
                  }}
            @section{Other stuff}
            @bold{2}
            3})

  (define (gen gen)
    (define (m stx)
      (syntax-case stx ()
        [(_ js-file)
         (string? (syntax-e #'js-file))
         (let ([src (syntax-source stx)])
           (define js
             (parameterize ([current-directory
                             (if (path-string? src)
                               (path-only src)
                               (current-directory))])
               (call-with-input-file (syntax-e #'js-file) read-json)))
           (gen js stx))]))
    m))

(define-syntax gapi-doc->racket-code   (gen gen-racket))
(define-syntax gapi-doc->scribble-code (gen gen-scribble))

(define-syntax-rule (require/gapi-doc gapi-doc ...)
  (begin (gapi-doc->racket-code gapi-doc) ...))
