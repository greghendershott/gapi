#lang racket

(require gapi/macro)
(require-gapi-doc "plus.v1.js")

(define js (paged (plus-people-search "Greg Henderson")))
(for/list ([x (hash-ref js 'items (hasheq))])
  (hash-ref x 'displayName))