#lang racket

(require (planet ryanc/webapi))   ;for OAuth
(require (planet gh/gapi/macro))  ;for Google web services...
(require-gapi-doc plus.v1.js)     ;..specifically Google+

;; For this example to work, you must go to the Google API Console and
;; create an application, such as "Racket GAPI Example". Put the
;; resulting client ID and client secret here:
(define my-client-id "put your client ID here")
(define my-client-secret "put your client secret here")

(define client (oauth2-client #:id my-client-id
                              #:secret my-client-secret))

(define scopes (list "https://www.googleapis.com/auth/plus.me"))

;; This will open a browser window so you can authorize.
(define auth (oauth2/request-auth-code/browser google-auth-server	 
                                               client	 
                                               scopes))

;; Use the access token. We can supply a userId of "me".
(define access-token (send auth get-access-token #:re-acquire? #t))
(plus-activities-list #:userId "me"
                      #:collection "public"
                      #:oauth_token access-token)
