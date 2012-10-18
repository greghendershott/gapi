#lang racket

(require (planet gh/gapi/dynamic))

;; Get the discovery document live from Google API Discovery Service:
(define plus (online-discovery-document->service "plus" "v1"))

;; OR: Get the discovery document from among those that are included
;; with this library. To indicate this, use a symbol not a string.
;;
;; (define plus (local-discovery-document->service 'plus.v1.js))
;;
;; OR: Get the discovery document from a local file:
;;
;; (define plus (local-discovery-document->service "path/to/plus.v1.js"))

;; Use the `service' object to create one or more procedures:
(define plus-people-search (method-proc plus 'people 'search))

;; Use the procedures:
(define js (paged (plus-people-search #:query "Greg Henderson")))
(for/list ([x (hash-ref js 'items (hasheq))])
  (hash-ref x 'displayName))
