A way to access Google APIs via
[Google API Discovery service](https://developers.google.com/discovery/). Use
the discovery tool in any of three ways:

1. _Dynamic_: Create a function wrapper at runtime, using either a
   JSON "discovery document" (either a local file or downloaded at
   runtime).

2. _.RKT and .SCRBL generation_: Generate a static `.rkt` file and a
   `.scrbl` file.

3. _Macros and .SCRBL generation_: Move the work to compile time via a
   macro (thank you for the assist, Eli Barzilay!). For example:
   
   ```
   ;; Define function wrappers for the web service documented in
   ;; the API discovery document urlshortener.v1.js.
   (require-gapi-doc "urlshortener.v1.js")

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
   
   => "http://www.racket-lang.org/" was shortened
      to "http://goo.gl/uAKH9", which expanded back to
      "http://www.racket-lang.org/": Yay!
   ```
   
   Static `.scrbl` files are generated, because there's really no need
   to do these at compile time. Instead they are needed to create
   `.html` files for PLaneT package preparation.


To use:

1. Get an API key from Google's
[API Console](https://code.google.com/apis/console/).

2. Put your Google API key in a `~/.google-api-key` file.

3. Use the approach you prefer.
