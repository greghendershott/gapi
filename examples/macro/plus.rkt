#lang racket

(require gapi/macro)
(require-gapi-doc "plus.v1.js")

(define js (plus.people.search "Greg Hendershott" #:key (api-key)))
(for/list ([x (hash-ref js 'items (hasheq))])
  (hash-ref x 'displayName))
