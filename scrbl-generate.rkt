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
  (displayln "@(require planet/scribble (for-label racket))\n")
  (printf "@title{~a ~a}\n" (hash-ref j 'title) (hash-ref j 'version))
  (displayln "@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}")
  (displayln (hash-ref j 'description))
  (printf "@hyperlink[\"~a\" \"Google documentation.\"]\n"
          (hash-ref j 'documentationLink))
  (displayln "@table-of-contents{}")
  (printf "@defmodule[gapi/macro]\n")
  (printf "@racket[(require-gapi-doc \"~a.~a.js\")]\n" (hash-ref j 'name) (hash-ref j 'version)))

(define (do-api-parameters j)
  (displayln "@section{API Parameters}")
  (displayln "The following optional keyword arguments may be passed to @italic{all} functions for this web service:")
    (printf "@defproc[(_\n")
  (for ([(k v) (hash-ref j 'parameters (hasheq))])
    (printf "[#:~a ~a string? ~a]\n" k k (cond [(eq? k 'key) "(api-key)"]
                                                 [else "'N/A"])))
  (displayln ") jsexpr?]{")
  (displayln "@margin-note{This is not actually a function. This is just using Scribble's defproc form to list the optional keyword arguments that may be passed to @italic{all} functions for this service.}")
  (for ([(k v) (hash-ref j 'parameters (hasheq))])
    (printf "@racket[~a]: ~a\n\n" k (hash-ref v 'description "")))
  (displayln "}")
  (newline))

(define (do-resources root j)
  (displayln "@section{Resources}")
  (for ([(k v) (in-hash j)]
        #:when (eq? k 'resources))
    (newline)
    (for ([(rn rv) v])
      (printf "@subsection{~a}\n" rn)
      (for ([(k v) (in-hash rv)])
        (match k
          ['methods (for ([(mn mv) (in-hash v)])
                      (do-method root mn mv))]
          ['resources (do-resources root v)]))))) ;sub-resources

(define (do-method root mn mv)
  (define name (string->symbol
                (regexp-replace* #rx"\\." (hash-ref mv 'id) "-")))
  (define api-params (hash-ref root 'parameters))
  (define api-param-names (hash-keys api-params))
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
  (printf "@defproc[(~a\n" name)
  (for ([(k v) req-params])
    (printf "[#:~a ~a string?]\n" k k))
  (for ([(k v) opt-params]
        #:when (not (hash-has-key? req-params k)))
    (printf "[#:~a ~a string? 'N/A]\n" k k))
  (for ([(k v) body-params]
        #:when (not (hash-has-key? req-params k)))
    (printf "[#:~a ~a string? 'N/A]\n" k k))
  (for ([(k v) api-params])
    (printf "[#:~a ~a string? ~a]\n" k k (cond [(eq? k 'key) "(api-key)"]
                                                 [else "'N/A"])))
  (displayln ") jsexpr?]{")
  (displayln (hash-ref mv 'description ""))
  (newline)
  (for ([(k v) req-params])
    (printf "@racket[~a]: ~a\n\n" k (hash-ref v 'description "")))
  (for ([(k v) opt-params]
        #:when (not (hash-has-key? req-params k)))
    (printf "@racket[~a]: ~a\n\n" k (hash-ref v 'description "")))
  (for ([(k v) body-params]
        #:when (not (hash-has-key? req-params k)))
    (printf "@racket[~a]: ~a\n\n" k (hash-ref v 'description "")))
  ;; Don't document the api-params here, over and over for every function.
  ;; Already documented up in their own section.
  (displayln "}")
  (newline))

(define (generate src-path dst-path name)
  (define js-pn (build-path src-path name))
  (define scrbl-pn (build-path dst-path (string-append name ".scrbl")))
  (with-output-to-file scrbl-pn
    (lambda ()
      (discovery-document->scribble-code
       (load-discovery-document js-pn)))
    #:mode 'text
    #:exists 'replace))

(define (build dst-path name)
  (parameterize ([current-directory dst-path])
    (system (string-append "scribble "
                           "++xref-in setup/xref load-collections-xref "
                           name ".scrbl"))))

(define (generate-and-build src-path dst-path name)
  (generate src-path dst-path name)
  (build dst-path name))

(define (generate-and-build-all src-path dst-path)
  (fold-files
   (lambda (fn what _)
     (cond [(eq? what 'file)
            (displayln fn)
            (generate-and-build src-path
                                dst-path
                                (path->string (file-name-from-path fn)))]))
   (void)
   src-path
   #f))

;; (generate-and-build "vendor" "scribble" "urlshortener.v1.js")
;; (generate-and-build "vendor" "scribble" "plus.v1.js")
;; (generate-and-build "vendor" "scribble" "licensing.v1.js")

(generate-and-build-all (build-path 'same "vendor")
                        (build-path 'same "scribble"))
