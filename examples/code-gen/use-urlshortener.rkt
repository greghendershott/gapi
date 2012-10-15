#lang racket

(require "urlshortener.rkt"
         rackunit)

(define orig-url "http://www.racket-lang.org/")

(define js-insert (urlshortener.url.insert #:longUrl orig-url
                                           #:key (api-key)))
(define short-url (dict-ref js-insert 'id))

(define js-get (urlshortener.url.get short-url
                                     #:key (api-key)))
(define long-url (dict-ref js-get 'longUrl))

(check-equal? orig-url long-url)
