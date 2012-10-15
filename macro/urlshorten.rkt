#lang racket

(require "../main.rkt")
(require "../macro-generate.rkt")

(js->racket-code "../vendor/urlshortener.v1.js")
;; (require/js "../vendor/urlshortener.v1.js")

(define orig-url "http://www.racket-lang.org/")
(define js-insert (urlshortener.url.insert #:longUrl orig-url
                                           #:key (api-key)))
(define short-url (dict-ref js-insert 'id))
(define js-get (urlshortener.url.get short-url
                                     #:key (api-key)))
(define long-url (dict-ref js-get 'longUrl))
(equal? orig-url long-url)

(js->scribble-code "../vendor/urlshortener.v1.js")
