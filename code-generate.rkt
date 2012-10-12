#lang racket

(require json
         "main.rkt"
         )

(provide discovery-document->racket-code)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code generation

;; Although generating code files seems extremely un-Rackety, I'm not
;; sure this is really possible to do with macros. Plus, the
;; code-generation approach at least gives library users some actual
;; code to look at, including documentation in the form of
;; comments. And could generate SCRBL, too. So...???

(define/contract (discovery-document->racket-code root)
  (jsexpr? . -> . any)
  (define (do-resources j)
    (for ([(k v) (in-hash j)]
          #:when (eq? k 'resources))
      (newline)
      (for ([(rn rv) v])
        (displayln (make-string 78 #\;))
        (printf ";; Functions for the `~a' resource:\n" rn)
        (for ([(k v) (in-hash rv)])
          (match k
            ['methods (for ([(mn mv) (in-hash v)])
                        (do-method root mn mv))]
            ['resources (do-resources v)] ;sub-resources
            [else (cond [(string? v) (printf "~a: ~a\n" k v)]
                        [else (displayln k)])])))))
  (do-intro root)
  (do-api-parameters root)
  (do-resources root))

(define (do-intro j)
  (displayln #reader scribble/reader
   @string-append{
    #lang racket
    (require net/url net/uri-codec)

    ;;@make-string[76 #\;]
    ;;
    ;; @hash-ref[j 'title] @hash-ref[j 'version]
    ;;
    ;; @hash-ref[j 'description]
    ;; 
    ;; Documentation: @hash-ref[j 'documentationLink]
    
   }
   ))

(define (do-api-parameters j)
  (define (cat ss)
    (apply string-append ss))
  (displayln #reader scribble/reader
   @string-append{
    ;;@make-string[76 #\;]
    ;; API parameters
    ;;
    ;; These keyword arguments may be passed to all functions.
    ;;
    #|
    @cat[(for/list ([(k v) (hash-ref j 'parameters (hasheq))])
           (cat (cons (format "\n#:~a\n" k)
                      (for/list ([(k v) v])
                        (wrap (format "~a: ~a\n" k v))))))]
    |#
    }))

(define (do-method root mn mv)
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
  (newline)
  (displayln "#|")
  (displayln name)
  ;; Parameters that are plain function arguments: req-params
  (for ([(k v) req-params])
    (newline)
    (printf "~a\n" k)
    (for ([(k v) v])
      (displayln (wrap (format "~a: ~a" k v)))))
  ;; Parameters that are keyword function arguments: opt-params and
  ;; body-params. (So are the API-wide params, but we don't document
  ;; them repeatedly for each function.)
  (for ([(k v) opt-params])
    (newline)
    (printf "#:~a\n" k)
    (for ([(k v) v])
      (displayln (wrap (format "~a: ~a" k v)))))
  (for ([(k v) body-params])
    (newline)
    (printf "#:~a\n" k)
    (for ([(k v) v])
      (displayln (wrap (format "~a: ~a" k v)))))
  (displayln "|#")
  (define qps (append req-param-names opt-param-names api-param-names))
  (pretty-print-code
   `(define (,name
             ,@req-param-names
             ,@(letrec ([flatter (lambda (xs)
                                   (match xs
                                     [(list (list a b) more ...)
                                      (cons a (cons b (flatter more)))]
                                     [(list) (list)]))])
                 (flatter (map (lambda (x)
                                 (list (string->keyword (symbol->string x))
                                       (list x ''NONE)))
                               all-opt-param-names)))
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
           ["PUT" `(put-pure-port url body h)]
           [else `(error ',name "TO-DO")]))
      (define js (bytes->jsexpr (port->bytes in)))
      (close-input-port in)
      js))
  (newline))

(define (pretty-print-code x) ;yeah, all just to nuke leading quote
  (display (substring (with-output-to-string (lambda () (pretty-print x))) 1)))

(define (wrap s [right 70] [indent 2])
  s) ;; TO-DO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Examples

(discovery-document->racket-code (load-discovery-document
                                  "vendor/urlshortener.v1.js"))
;; (discovery-document->racket-code (load-discovery-document
;;                                   "vendor/plus.v1.js"))
