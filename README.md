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
   ```
   
   Static `.scrbl` files are generated, because there's really no need
   to do these at compile time. Instead they are needed to create
   `.html` files for PLaneT package preparation.


To use:

1. Get an API key from Google's
[API Console](https://code.google.com/apis/console/).

2. Put your Google API key in a `~/.google-api-key` file.

3. Use the approach you prefer.
