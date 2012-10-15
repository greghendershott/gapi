#lang racket

(require "../macro-generate.rkt")

(gapi-doc->racket-code "../vendor/urlshortener.v1.js")
;; (require/gapi-doc "../vendor/urlshortener.v1.js")

(define orig-url "http://www.racket-lang.org/")
(define js-insert (urlshortener.url.insert #:longUrl orig-url
                                           #:key (api-key)))
(define short-url (dict-ref js-insert 'id))
(define js-get (urlshortener.url.get short-url
                                     #:key (api-key)))
(define long-url (dict-ref js-get 'longUrl))
(printf "~s was shortened to ~s, which expanded back to ~s: ~a"
        orig-url short-url long-url
        (if (equal? orig-url long-url) "Yay!" "Boo!"))

;;(js->scribble-code "../vendor/urlshortener.v1.js")
