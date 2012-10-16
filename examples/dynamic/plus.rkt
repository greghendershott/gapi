#lang racket

(require gapi/dynamic)
(define plus (local-discovery-document->service "../../vendor/plus.v1.js"))
(define plus-people-search (method-proc plus 'people 'search))
(define js (paged (plus-people-search #:query "Greg Henderson")))
(for/list ([x (hash-ref js 'items (hasheq))])
  (hash-ref x 'displayName))
