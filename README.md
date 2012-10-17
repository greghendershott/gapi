# Google web services in Racket

A way to access 40+ Google APIs via
[Google API Discovery service](https://developers.google.com/discovery/).

The Google API Discovery service is a web service that tells you about
web services.  Each web service is described using a JSON format
"discovery document", which describes such things as the resources,
methods, and parameters for the web service.  It is possible to use
this document programatically to create Racket functions that will
make HTTP requests to the web service, as well as to make
documentation for the service. This is a great idea. And to be
appropriately meta, one of the services you can discover with the
discovery service, is the discovery service itself.

This library provides two approaches to using the discovery service
with Racket:

1. "Require" the discovery document like a source file. The wrapper
functions are created at compile time. The discovery documents are not
used at runtime and need not be shipped with your application;
essentially they are source code just like your `.rkt` files.

2. Dynamically load and parse the discovery docuemnt (from a local
file or from the Discovery web service) at runtime.

Unless you need runtime dynamism, the first approach is easier, more
efficient, and simpler for distributing your application.

The functions created by either approach are identical in terms of the
arguments they accept and the value they return.  As a result you
should be able to change approaches without modifying much of your
code.

## Macros and .SCRBL generation

The simplest approach creates wrapper functions for the web service
at compile time. This reduces startup time for your
application. Furthermore, it means that you don't need to ship the
service documents---they are "source code" just like your `.rkt`
files. (Huge thanks to Eli Barzilay for pushing me through the
macro learning crucible to do this!)

    #lang racket

    (require (planet gh/gapi/macro))
    (require-gapi-doc "urlshortener.v1.js")

    (define orig-url "http://www.racket-lang.org/")
    (define js-insert (urlshortener-url-insert #:longUrl orig-url))
    (define short-url (dict-ref js-insert 'id))
    (define js-get (urlshortener-url-get #:shortUrl short-url))
    (define long-url (dict-ref js-get 'longUrl))
    (printf "~s was shortened to ~s, which expanded back to ~s: ~a"
            orig-url short-url long-url
            (if (equal? orig-url long-url) "Yay!" "Boo!"))

    => "http://www.racket-lang.org/" was shortened
       to "http://goo.gl/uAKH9", which expanded back to
       "http://www.racket-lang.org/": Yay!

Static `.scrbl` files are generated, because there's really no need
to do these at compile time. Instead they are needed to create
`.html` files for PLaneT package preparation.

## Dynamic

Another way to use the API discovery service is to query it at
runtime, or to parse a locally-stored discovery file at runtime. This
may be desirable if you want to be able to discover newly-added
services, for instance.

    #lang racket

    (require (planet gh/gapi/dynamic))

    ;; Create a `service?' object from the API discovery document:
    (define goo.gl (local-discovery-document->service
                    "../../vendor/urlshortener.v1.js"))
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

    => "http://www.racket-lang.org/" was shortened
       to "http://goo.gl/uAKH9", which expanded back to
       "http://www.racket-lang.org/": Yay!

## Documentation

For full documentation, see
[PLaneT](http://planet.racket-lang.org/display.ss?package=gapi.plt&owner=gh).

