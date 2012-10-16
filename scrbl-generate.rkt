#lang at-exp racket


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
  (displayln
   @string-append{#lang scribble/manual
   @"@"(require planet/scribble (for-label racket))
   @"@"title{@(hash-ref j 'title) @(hash-ref j 'version)}
   @"@"margin-note{This documentation has been automatically generated
                        using information supplied by the
                        Google API Discovery service.}
   @(hash-ref j 'description)
   @"@"hyperlink["@(hash-ref j 'documentationLink)" "Google documentation."]
   @"@"table-of-contents{}
   @"@"defmodule[gapi/macro]
   @"@"racket[(require-gapi-doc "@(hash-ref j 'name).@(hash-ref j 'version).js")]
   }))

(define (do-api-parameters j)
  (displayln
   @string-append{
   @"@"section{API Parameters}
   The following optional keyword arguments may be passed to
   @"@"italic{all} functions for this web service:
   @"@"defproc[(_
   @string-append*{
     @(for/list ([(k v) (hash-ref j 'parameters (hasheq))])
        (format "[#:~a ~a string? ~a]\n" k k (cond [(eq? k 'key) "(api-key)"]
                                                   [else "'N/A"])))
     }
   ) jsexpr?]{
   @"@"margin-note{This is not actually a function. This is just using
                        Scribble's defproc form to list the optional
                        keyword arguments that may be passed
                        to @"@"italic{all} functions for this service.}
   @string-append*{
     @(for/list ([(k v) (hash-ref j 'parameters (hasheq))])
        (format "@racket[~a]: ~a\n\n" k (hash-ref v 'description "")))
     }
   }
   }))

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

(define (generate name)
  (define js-pn (build-path 'same "vendor" name))
  (define scrb-pn (build-path 'same "genscribs" (string-append name ".scrbl")))
  (with-output-to-file scrb-pn
    (lambda ()
      (discovery-document->scribble-code (load-discovery-document js-pn)))
    #:mode 'text
    #:exists 'replace))

(define (build name)
  (define scrib-pn
    (path->string (build-path 'same "genscribs" (string-append
                                                 name ".scrbl"))))
  (define html-dir (path->string (build-path 'same "planet-docs" "manual")))
  (system (string-append
           "scribble "
           "++xref-in setup/xref load-collections-xref "
           "--dest " html-dir " "
           scrib-pn)))

(define (generate-and-build name)
  (generate name)
  (build name))

(define (generate-and-build-all)
  (fold-files
   (lambda (fn what _)
     (cond [(eq? what 'file)
            (displayln fn)
            (generate-and-build (path->string (file-name-from-path fn)))]))
   (void)
   (build-path 'same "vendor")
   #f))

;; (generate-and-build "urlshortener.v1.js")
;; (generate-and-build "plus.v1.js")
;; (generate-and-build "licensing.v1.js")

;; (generate-and-build-all)
