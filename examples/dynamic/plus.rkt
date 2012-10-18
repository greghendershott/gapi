#lang racket

(require (planet gh/gapi/dynamic))
(define plus (online-discovery-document->service "plus" "v1"))
(define plus-people-search (method-proc plus 'people 'search))
(define js (paged (plus-people-search #:query "Greg Henderson")))
(for/list ([x (hash-ref js 'items (hasheq))])
  (hash-ref x 'displayName))
