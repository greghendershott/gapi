#lang at-exp racket

(provide js->racket-code
         js->scribble-code
         require/js)

(require (for-syntax racket/path racket/match json)
         scribble/base)

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
    (define body-params
      (hash-ref (hash-ref (hash-ref root 'schemas)
                          (string->symbol
                           (hash-ref (hash-ref mv 'request (hash)) '$ref ""))
                          (hash))
                'properties
                (hash)))
    (define body-param-names (hash-keys body-params))
    (define all-opt-param-names (append opt-param-names
                                        body-param-names
                                        api-param-names))
    (define qps (append req-param-names opt-param-names api-param-names))
    #`(define (#,(datum->syntax stx name)
               #,@req-param-names
               #,@(letrec ([flatter (lambda (xs)
                                      (match xs
                                        [(list (list a b) more ...)
                                         (cons a (cons b (flatter more)))]
                                        [(list) (list)]))])
                    (flatter (map (lambda (x)
                                    (list (string->keyword (symbol->string x))
                                          (list x ''NONE)))
                                  all-opt-param-names)))
               )
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
        (define body (jsexpr->bytes
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

  (define (js-gen gen)
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

(define-syntax js->racket-code   (js-gen gen-racket))
(define-syntax js->scribble-code (js-gen gen-scribble))

(define-syntax-rule (require/js jsfile ...)
  (begin (js->racket-code jsfile) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

