A way to access Google APIs via
[Google API Discovery service](https://developers.google.com/discovery/). Use
the discovery tool in any of three ways:

1. _Dynamic_: Create a function wrapper at runtime, using a JSON
   "discovery document" (either a local file or downloaded at
   runtime). For example:
   
   ```
   (require gapi/dynamic)
   (define goo.gl (local-discovery-document->service
                   "vendor/urlshortener.v1.js"))
   (define url-insert (method-proc goo.gl 'url 'insert))
   (define url-get (method-proc goo.gl 'url 'get))

   (define orig-url "http://www.racket-lang.org/")
   (define shrink (url-insert #:longUrl long-url))
   (define short-url (dict-ref shrink 'id))
   (define expand (url-get #:shortUrl short-url))
   (define long-url (dict-ref expand 'longUrl))
   (printf "~s was shortened to ~s, which expanded back to ~s: ~a"
           orig-url short-url long-url
           (if (equal? orig-url long-url) "Yay!" "Boo!"))

   => "http://www.racket-lang.org/" was shortened
      to "http://goo.gl/uAKH9", which expanded back to
      "http://www.racket-lang.org/": Yay!
   ```

2. _.RKT and .SCRBL generation_: Generate a static `.rkt` file and a
   `.scrbl` file. This is deprecated. Instead use:

3. _Macros and .SCRBL generation_: Move the work to compile time via a
   macro (thank you for the assist, Eli Barzilay!). For example:
   
   ```
   (require gapi/macro)
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
   ```
   
   Static `.scrbl` files are generated, because there's really no need
   to do these at compile time. Instead they are needed to create
   `.html` files for PLaneT package preparation.


To use:

1. Get an API key from Google's
[API Console](https://code.google.com/apis/console/).

2. Put your Google API key in a `~/.google-api-key` file.

3. Use the approach you prefer.
