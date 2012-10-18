#lang racket

(require (planet gh/gapi/macro)
         xml
         json)

(require-gapi-doc plus.v1.js)
(require-gapi-doc urlshortener.v1.js)

;; Given a Google+ user ID, create an Atom feed of their recent public
;; posts.
;;
;; Uses goo.gl to shorten any URIs. Helpful in case the Atom feed may be
;; pushed to something like Twitter or App.net.
;;
;; Atom feeds want a Title, but G+ posts don't have one. Make a title
;; based on convention (such as the first line).
;;
;; The Atom field's Title and Content may each be emitted as plain
;; text or at HTML.
;;
;; Tip: The G+ ID is LONG-NUMBER in a URI like such as
;; https://plus.google.com/u/0/LONG-NUMBER/posts
;;
(define (feed uid [title-as-text? #f] [body-as-text? #f])
  (define js (plus-activities-list #:userId uid
                                   #:collection "public"))
  (define x (gplus->atom-feed-flexpr (json->gplus js)
                                     #:title-as-text? title-as-text?
                                     #:content-as-text? body-as-text?))
  ;; If you want actual XML text, un-comment the following line:
  ;;(xexpr->string x)
  x)

;;;
;;; Example use:
;;;
(define racket-uid "103883747126741038443") ;Racket page on G+
(feed racket-uid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (shorten-uri long)
  (define js (urlshortener-url-insert #:longUrl long))
  (define short (dict-ref js 'id))
  (printf "Shortened ~a to ~a\n" long short)
  short)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A flexpr is more flexible than an xexpr. It permits more than
;; string? in the value or "body" positions.  For example (p () "a" 1)
;; is permissible for a flexpr whereas it's not a valid xexpr.
;;
;; So what?  1. This avoids tedious number->string conversion in your
;; response code.  2. Even more importantly, it avoids forcing numbers
;; to strings when the ultimate destination is JSON not XML -- i.e. an
;; xexpr->jsexpr -- and you would want an actual number not a string.
;; (Not that I'm making use of 2 in this example file.)

(define (flexpr? jx)
  (match jx
    [(list k (list (list aks avs) ...) vs ...)
     (and (symbol? k)
          (andmap symbol? aks)
          (andmap flexpr-value? avs)
          (andmap flexpr-value-or-flexpr? vs))]
    [(list k  vs ...)
     (and (symbol? k)
          (andmap flexpr-value-or-flexpr? vs))]
    [else #f]))

(define (flexpr-value? v)
  (or (string? v)
      (symbol? v)
      (number? v)))

(define (flexpr-value-or-flexpr? v)
  (or (flexpr-value? v)
      (flexpr? v)))

(define (flexpr->xexpr x)
  ;; (flexpr? . -> . xexpr?)
  (match x
    [(list k (list (list aks avs) ...) vs ...)
     `(,k (,@(map (lambda (k v)
                    (list k (xexpr-value v)))
                aks avs))
         ,@(map (lambda (v)
                  (cond
                   [(flexpr-value? v) (xexpr-value v)]
                   [else (flexpr->xexpr v)]))
                vs))]
    [(list k vs ...)
     `(,k ,@(map (lambda (v)
                  (cond
                   [(flexpr-value? v) (xexpr-value v)]
                   [else (flexpr->xexpr v)]))
                vs))]
    [else (raise-type-error 'flexpr->xexpr "flexpr?" x)]))
             
(define flexpr->string (compose1 xexpr->string flexpr->xexpr))

(define (xexpr-value x)
  (cond
   [(string? x) x]
   [(symbol? x) x]
   [else (format "~a" x)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a jsexpr representing the JSON of a Google+ activity feed,
;; return a `gplus' struct.
(struct gplus (title self-uri id etag updated posts) #:transparent)
(define/contract (json->gplus x)
  (jsexpr? . -> . gplus?)
  (gplus (hash-ref x 'title)
         (hash-ref x 'selfLink)
         (hash-ref x 'id)
         (hash-ref x 'etag)
         (hash-ref x 'updated)
         (map item->post (hash-ref x 'items))))

;; Given a jsexpr representing a Google+ activity feed item, return a
;; `post' struct.
(struct post (title body uri updated id) #:transparent)
(define/contract (item->post x)
  (jsexpr? . -> . post?)
  (define-values (title body) (find-title&body x))
  (post title
        (enrich-body x body)
        (hash-ref x 'url)
        (hash-ref x 'updated)
        (hash-ref x 'id)))

;; Given a jsexpr representing a Google+ activity feed item, extract
;; its "content".  The interesting content varies depending on whether
;; this item is a `share' vs. a normal `post' or `checkin'.
(define/contract (get-content x)
  (jsexpr? . -> . string?)
  (define verb (hash-ref x 'verb))
  (define object (hash-ref x 'object))
  (define original-content (hash-ref object 'content))
  (match verb
    ["share"
     (string-append
      (hash-ref x 'annotation)
      "<br /><hr />"
      "<a href='" (hash-ref (hash-ref object 'actor) 'url) "'>"
      (hash-ref (hash-ref object 'actor) 'displayName) "</a>"
      " originally shared this post:<br />"
      original-content)]
    [else original-content]))

;; Google+ doesn't have the concept of a title vs. body. Instead there
;; are certain formatting conventions, such as the first line is the
;; title. If there's no linebreak, take the first 100 characters as
;; the title, and the rest as the body.
(define/contract (find-title&body x)
  (jsexpr? . -> . (values string? string?))
  (define content (get-content x))
  (match content
    [(pregexp "^(.+?)\\s*(?i:<br>|<br/>|<br />)+\\s*(.*?)$"
              (list _ title body))
     (values title body)]
    [(pregexp "^(.{1,100})(.*?)$"
              (list _ title body))
     (values (if (equal? "" body)
                 title
                 (string-append title "..."))
             body)]
    [else
     ;; No content at all, on the post? Use the title G+
     ;; picked. However if that's "", then try to find an attachment
     ;; with a displayName and use that.
     (define title
       (or (if (string=? (hash-ref x 'title "") "")
               #f
               (hash-ref x 'title ""))
           (for/or ([a (in-list (hash-refs x 'object 'attachments '()))])
               (hash-ref a 'displayName #f))
           "Untitled post"))
     (values title content)]))

(define/contract (enrich-body j body)
  (jsexpr? string? . -> . string?)
  (for/fold ([body body])
      ([x (in-list (hash-refs j 'object 'attachments '()))])
    (string-append
     body
     (match (hash-ref x 'objectType)
       ["article"
        (flexpr->string
         `(blockquote
           ()
           (hr ())
           (a ([href ,(hash-ref x 'url "")])
              ,(hash-ref x 'displayName))
           (br ())
           ,(hash-ref x 'content "")))]
       ["photo"
        (flexpr->string
         `(blockquote
           ()
           (hr ())
           (a ([href ,(hash-ref j 'url "")]) ;post uri, not full photo
              (img ([src ,(hash-refs x 'image 'url "")]
                    [height ,(hash-refs x 'image 'height "")]
                    [width ,(hash-refs x 'image 'width "")])))))]
       ["video"
        ;; I'd tried `embed' with autoplay=false but it still
        ;; auto-played. Gah. Instead just show the thumbnail image and
        ;; link to the video.
        (flexpr->string
         `(blockquote
           ()
           (hr ()
               (a ([href ,(hash-ref x 'url "")])
                  ,(hash-ref x 'displayName ""))
               (br ())
               ,(hash-ref x 'content "")
               (br ())
               (a ([href ,(hash-ref x 'url "")])
                  (img ([src ,(hash-refs x 'image 'url "")]))))))]
       [else ""]))))

;; hash-refs is to avoid nested (hash-ref (hash-ref h0 k0) k1 ...)
;; Like hash-ref, but accepts a list of keys, and assumes the hash-ref
;; of each produces a hash? with which to do a hash-ref using the next
;; key.
(define (hash-refs h . xs)
  (match xs
    [(list ks ..1 default)
     (with-handlers ([exn:fail? (lambda (exn) default)])
       (for/fold ([h h])
           ([k ks])
         (hash-ref h k)))]
    [else (error 'hash-refs "must supply hash?, key(s), default")]))

(define/contract (gplus->atom-feed-flexpr g
                                          #:title-as-text? title-as-text?
                                          #:content-as-text? content-as-text?)
  (gplus?
   #:title-as-text? boolean?
   #:content-as-text? boolean?
   . -> . flexpr?)
  `(feed
    ([xmlns "http://www.w3.org/2005/Atom"]
     [xml:lang "en"])
    (title ([type "text"]) ,(gplus-title g))
    (link ([href ,(gplus-self-uri g)]
           [rel "self"]))
    (link ([href ,(gplus-self-uri g)]))
    (id () ,(gplus-id g))
    (etag () ,(gplus-etag g))
    (updated () ,(gplus-updated g))
    ,@(for/list ([p (in-list (gplus-posts g))])
        (define title (shorten-any-uris (post-title p)))
        (define body (shorten-any-uris (post-body p)))
        `(entry ()
                ,(if title-as-text?
                     `(title ([type "text"]) ,(html->text title))
                     `(title ([type "html"]) ,title))
                (link ([rel "alternate"]
                       [href ,(post-uri p)]))
                (id () ,(post-id p))
                (published () ,(post-updated p))
                (updated () ,(post-updated p))
                ,(if content-as-text?
                     `(content ([type "text"]) ,(html->text body))
                     `(content ([type "html"]) ,body))))))

;; This isn't battle-tested, but, for example:
(define (shorten-any-uris s)
  (regexp-replace* #rx"http://[^ \"'<>]+"
                   s
                   shorten-uri))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define hr-string (format "\n~a\n" (make-string 10 #\-)))

;; Remove HTML tags and ampersand encodings. Produce something roughly
;; like markdown.
(define/contract (html->text s)
  (string? . -> . string?)
  (let* ([s (regexp-replace* #px"&quot;" s "\"")]
         [s (regexp-replace* #px"&amp;" s "\\&")]
         [s (unescape/ampersand s)]     ;do AFTER the &amp; ==> &
         [s (regexp-replace* #px"(?i:<br\\s*/?>)" s "\n")]
         [s (regexp-replace* #px"(?i:</p>)" s "\n\n")]
         [s (regexp-replace* #px"(?i:</?i>)" s "_")]
         [s (regexp-replace* #px"(?i:</?b>)" s "*")]
         [s (regexp-replace* #px"(?i:<hr\\s*/?>)" s hr-string)]
         [s (regexp-replace* #px"<a href='(.+?)'>(.*?)</a>" s
                             "\"\\2\" [\\1]")]
         [s (regexp-replace* #px"<.+?>" s "")])
    s))

(define/contract (unescape/ampersand str)
  (string? . -> . string?)
  (let loop ([str str])
    (match str
      [(pregexp "^(.*?)&#([a-fA-F0-9]{2});(.*?)$" (list _ before n after))
       (loop (string-append before
                            (make-string 1
                                         (integer->char (string->number n 10)))
                            after))]
      [else str])))
