#lang racket

(require (planet gh/gapi/macro))
(require-gapi-doc shopping.v1.js)

;; Caveat: I don't have a great understanding of this Google service,
;; so this is not that interesting an example.

(define num-results 10)

(define js (shopping-products-list #:source "public"
                                   #:country "US"
                                   #:facets.enabled "true"
                                   #:facets.include "brand:10"
                                   #:q "racketeering"
                                   #:maxResults (number->string num-results)))

(displayln "Facets:")
(define facets (hash-ref js 'facets))
(for ([facet facets])
  ;;(printf "~a: ~a\n" (hash-ref facet 'displayName) (hash-ref facet 'count))
  (displayln "Top Ten Brands")
  (for ([bucket (hash-ref facet 'buckets)])
    (printf "~a: ~a\n" (hash-ref bucket 'value) (hash-ref bucket 'count))))

(printf "First ~a results:\n" num-results)
(define items (hash-ref js 'items '()))
(for/list ([item items])
  (define product (hash-ref item 'product (hasheq)))
  (list (hash-ref product 'title "")
        (hash-ref product 'description "")))
