#lang racket

(require (for-syntax racket/path
                     racket/match
                     racket/list
                     json
                     "dynamic.rkt")
         net/url
         json
         net/uri-codec
         "dynamic.rkt")

(provide require-gapi-doc
         api-key
         (all-from-out "dynamic.rkt"))

(define-for-syntax (proc-name stx method)
  (define id (hash-ref method 'id))
  (define sym (string->symbol (regexp-replace* #rx"\\." id "-")))
  (datum->syntax stx sym))

(define-syntax (require-gapi-doc stx)
  (syntax-case stx ()
    [(_ file)
     (or (string? (syntax-e #'file))
         (symbol? (syntax-e #'file)))
     (let ([dd (load-discovery-document (syntax-e #'file))])
       (with-syntax ([dd/stx (datum->syntax stx dd)])
         #`(begin
             (define service (discovery-document->service dd/stx))
             #,@(append*
                 (for/list ([(rn rv) (hash-ref dd 'resources)])
                   (for/list ([(mn mv) (hash-ref rv 'methods)])
                     (with-syntax ([name (proc-name stx mv)])
                       #`(begin
                           (define
                             name
                             (method-proc service (quote #,rn) (quote #,mn)))
                           (provide name)))))))))]))
