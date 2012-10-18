#lang racket

(require (planet gh/gapi/macro))
(require-gapi-doc plus.v1.js)

(define js (paged (plus-people-search #:query "Greg Henderson")))
(for/list ([x (hash-ref js 'items (hasheq))])
  (hash-ref x 'displayName))
