#lang racket


;; @"@"something{foo}

;; @rx|{asd @foo fasdf}|


(require json
         "main.rkt")

(provide discovery-document->scribble-code)

(define/contract (discovery-document->scribble-code root)
  (jsexpr? . -> . any)
  (do-intro root)
  (do-api-parameters root)
  (do-resources root root))

(define (do-intro j)
  (displayln "#lang scribble/manual")
  (displayln "(require planet/scribble)")
  (printf "@title {~a ~a}\n" (hash-ref j 'title) (hash-ref j 'version))
  (displayln (hash-ref j 'description))
  (printf "@hyperlink[\"~a\" Documentation link]\n"
          (hash-ref j 'documentationLink))
  (displayln "@table-of-contents"))

(define (do-api-parameters j)
  (displayln "@section{API Parameters}")
  (displayln "These keyword arguments may be passed to all functions")
  (for ([(k v) (hash-ref j 'parameters (hasheq))])
     (printf "\n#:~a\n" k)
     (for ([(k v) v])
        (format "~a: ~a\n" k v))))

(define (do-resources root j)
  (for ([(k v) (in-hash j)]
        #:when (eq? k 'resources))
    (newline)
    (for ([(rn rv) v])
      (printf "@subsection{Functions for the `~a' resource:}\n" rn)
      (for ([(k v) (in-hash rv)])
        (match k
          ['methods (for ([(mn mv) (in-hash v)])
                      (do-method root mn mv))]
          ['resources (do-resources root v)]))))) ;sub-resources

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
  (define qps (append req-param-names opt-param-names api-param-names))
  (define args "to-do")
  
  (define return "to-do")
  (define doc (hash-ref mv 'description))
  (printf "@defproc[(~a\n" name)
  (for ([(k v) req-params])
    (printf "[~a string?]\n" k))
  (for ([(k v) opt-params])
    (printf "[#:~a ~a string? 'N/A]\n" k k))
  (for ([(k v) body-params])
    (printf "[#:~a ~a string? 'N/A]\n" k k))
  (printf ") jsexpr?]{\n~a\n}\n" doc)
  (newline))

(with-output-to-file "examples/urlshortener.scrbl"
  (lambda ()
    (discovery-document->scribble-code
     (load-discovery-document "vendor/urlshortener.v1.js")))
  #:mode 'text
  #:exists 'replace)

     
    
