#lang racket

(require gapi/macro)
(require-gapi-doc "urlshortener.v1.js")

(define orig-url "http://www.racket-lang.org/")
(define js-insert (urlshortener-url-insert #:longUrl orig-url))
(define short-url (dict-ref js-insert 'id))
(define js-get (urlshortener-url-get #:short-url short-url))
(define long-url (dict-ref js-get 'longUrl))
(printf "~s was shortened to ~s, which expanded back to ~s: ~a"
        orig-url short-url long-url
        (if (equal? orig-url long-url) "Yay!" "Boo!"))
