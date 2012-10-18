#lang racket

(require (planet gh/gapi/dynamic))

;; Create a `service?' object from the API discovery document:
(define goo.gl (online-discovery-document->service "urlshortener" "v1"))
;; Make procedures, each corresponding to a resource and method:
(define urlshortener-url-insert (method-proc goo.gl 'url 'insert))
(define urlshortener-url-get (method-proc goo.gl 'url 'get))

;; Use them:
(define orig-url "http://www.racket-lang.org/")
(define js-insert (urlshortener-url-insert #:longUrl orig-url))
(define short-url (dict-ref js-insert 'id))
(define js-get (urlshortener-url-get #:shortUrl short-url))
(define long-url (dict-ref js-get 'longUrl))
(printf "~s was shortened to ~s, which expanded back to ~s: ~a"
        orig-url short-url long-url
        (if (equal? orig-url long-url) "Yay!" "Boo!"))
