#lang racket
(require net/url net/uri-codec json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; URL Shortener API v1
;;
;; Lets you create, inspect, and manage goo.gl short URLs
;;
;; Documentation: http://code.google.com/apis/urlshortener/v1/getting_started.html

(define (read-api-key
          (file (build-path (find-system-path 'home-dir) ".google-api-key")))
   (match
    (file->string file #:mode 'text)
    ((regexp "^\\s*(.*?)\\s*(?:[\r\n]*)$" (list _ k)) k)
    (else (error 'read-api-key "Bad format for ~a" file))))
(define api-key (make-parameter (read-api-key)))
(provide api-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API parameters
;;
;; These keyword arguments may be passed to all functions.
;;
#|

#:fields
type: string
description: Selector specifying which fields to include in a partial response.
location: query

#:key
type: string
description: API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
location: query

#:alt
type: string
description: Data format for the response.
location: query
enum: (json)
enumDescriptions: (Responses with Content-Type of application/json)
default: json

#:oauth_token
type: string
description: OAuth 2.0 token for the current user.
location: query

#:prettyPrint
type: boolean
description: Returns response with indentations and line breaks.
location: query
default: true

#:quotaUser
type: string
description: Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters. Overrides userIp if both are provided.
location: query

#:userIp
type: string
description: IP address of the site where the request originates. Use this if you want to enforce per-user limits.
location: query

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for the `url' resource:

#|
urlshortener.url.list

Retrieves a list of URLs shortened by a user.

Arguments:

#:projection
type: string
description: Additional information to return.
location: query
enum: (ANALYTICS_CLICKS FULL)
enumDescriptions: (Returns short URL click counts. Returns short URL click counts.)

#:start-token
type: string
description: Token for requesting successive pages of results.
location: query
|#
(provide urlshortener.url.list)
(define (urlshortener.url.list
          #:projection
          (projection 'NONE)
          #:start-token
          (start-token 'NONE)
          #:fields
          (fields 'NONE)
          #:key
          (key 'NONE)
          #:alt
          (alt 'NONE)
          #:oauth_token
          (oauth_token 'NONE)
          #:prettyPrint
          (prettyPrint 'NONE)
          #:quotaUser
          (quotaUser 'NONE)
          #:userIp
          (userIp 'NONE))
   (define base-uri "https://www.googleapis.com/urlshortener/v1/")
   (define res-path "url/history")
   (define _qpstr
     (alist->form-urlencoded
      (filter-map
       (lambda (k v)
         (cond ((eq? v 'NONE) #f) (else (cons (string->symbol k) v))))
       (list
        "projection"
        "start-token"
        "fields"
        "key"
        "alt"
        "oauth_token"
        "prettyPrint"
        "quotaUser"
        "userIp")
       (list
        projection
        start-token
        fields
        key
        alt
        oauth_token
        prettyPrint
        quotaUser
        userIp))))
   (define qpstr
     (cond ((equal? _qpstr "") "") (else (string-append "?" _qpstr))))
   (define url (string->url (string-append base-uri res-path qpstr)))
   (define h (list "Content-Type: application/json"))
   (define body
     (jsexpr->bytes
      (for/hasheq
       ((k (list)) (v (list)) #:when (not (eq? v 'NONE)))
       (values (string->symbol k) v))))
   (define in (get-pure-port url h))
   (define js (bytes->jsexpr (port->bytes in)))
   (close-input-port in)
   js)


#|
urlshortener.url.get

Expands a short URL or gets creation time and analytics.

Arguments:

shortUrl
type: string
description: The short URL, including the protocol.
location: query
required: #t

#:projection
type: string
description: Additional information to return.
location: query
enum: (ANALYTICS_CLICKS ANALYTICS_TOP_STRINGS FULL)
enumDescriptions: (Returns only click counts. Returns only top string counts. Returns the creation timestamp and all available analytics.)
|#
(provide urlshortener.url.get)
(define (urlshortener.url.get
          shortUrl
          #:projection
          (projection 'NONE)
          #:fields
          (fields 'NONE)
          #:key
          (key 'NONE)
          #:alt
          (alt 'NONE)
          #:oauth_token
          (oauth_token 'NONE)
          #:prettyPrint
          (prettyPrint 'NONE)
          #:quotaUser
          (quotaUser 'NONE)
          #:userIp
          (userIp 'NONE))
   (define base-uri "https://www.googleapis.com/urlshortener/v1/")
   (define res-path "url")
   (define _qpstr
     (alist->form-urlencoded
      (filter-map
       (lambda (k v)
         (cond ((eq? v 'NONE) #f) (else (cons (string->symbol k) v))))
       (list
        "shortUrl"
        "projection"
        "fields"
        "key"
        "alt"
        "oauth_token"
        "prettyPrint"
        "quotaUser"
        "userIp")
       (list
        shortUrl
        projection
        fields
        key
        alt
        oauth_token
        prettyPrint
        quotaUser
        userIp))))
   (define qpstr
     (cond ((equal? _qpstr "") "") (else (string-append "?" _qpstr))))
   (define url (string->url (string-append base-uri res-path qpstr)))
   (define h (list "Content-Type: application/json"))
   (define body
     (jsexpr->bytes
      (for/hasheq
       ((k (list)) (v (list)) #:when (not (eq? v 'NONE)))
       (values (string->symbol k) v))))
   (define in (get-pure-port url h))
   (define js (bytes->jsexpr (port->bytes in)))
   (close-input-port in)
   js)


#|
urlshortener.url.insert

Creates a new short URL.

Arguments:

#:id
type: string
description: Short URL, e.g. "http://goo.gl/l6MS".

#:longUrl
type: string
description: Long URL, e.g. "http://www.google.com/". Might not be present if the status is "REMOVED".

#:kind
type: string
description: The fixed string "urlshortener#url".
default: urlshortener#url

#:status
type: string
description: Status of the target URL. Possible values: "OK", "MALWARE", "PHISHING", or "REMOVED". A URL might be marked "REMOVED" if it was flagged as spam, for example.

#:analytics
description: A summary of the click analytics for the short and long URL. Might not be present if not requested or currently unavailable.
$ref: AnalyticsSummary

#:created
type: string
description: Time the short URL was created; ISO 8601 representation using the yyyy-MM-dd'T'HH:mm:ss.SSSZZ format, e.g. "2010-10-14T19:01:24.944+00:00".
|#
(provide urlshortener.url.insert)
(define (urlshortener.url.insert
          #:id
          (id 'NONE)
          #:longUrl
          (longUrl 'NONE)
          #:kind
          (kind 'NONE)
          #:status
          (status 'NONE)
          #:analytics
          (analytics 'NONE)
          #:created
          (created 'NONE)
          #:fields
          (fields 'NONE)
          #:key
          (key 'NONE)
          #:alt
          (alt 'NONE)
          #:oauth_token
          (oauth_token 'NONE)
          #:prettyPrint
          (prettyPrint 'NONE)
          #:quotaUser
          (quotaUser 'NONE)
          #:userIp
          (userIp 'NONE))
   (define base-uri "https://www.googleapis.com/urlshortener/v1/")
   (define res-path "url")
   (define _qpstr
     (alist->form-urlencoded
      (filter-map
       (lambda (k v)
         (cond ((eq? v 'NONE) #f) (else (cons (string->symbol k) v))))
       (list
        "fields"
        "key"
        "alt"
        "oauth_token"
        "prettyPrint"
        "quotaUser"
        "userIp")
       (list fields key alt oauth_token prettyPrint quotaUser userIp))))
   (define qpstr
     (cond ((equal? _qpstr "") "") (else (string-append "?" _qpstr))))
   (define url (string->url (string-append base-uri res-path qpstr)))
   (define h (list "Content-Type: application/json"))
   (define body
     (jsexpr->bytes
      (for/hasheq
       ((k (list "id" "longUrl" "kind" "status" "analytics" "created"))
        (v (list id longUrl kind status analytics created))
        #:when
        (not (eq? v 'NONE)))
       (values (string->symbol k) v))))
   (define in (post-pure-port url body h))
   (define js (bytes->jsexpr (port->bytes in)))
   (close-input-port in)
   js)

