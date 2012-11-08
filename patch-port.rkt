#lang racket

(require net/url)
(provide patch-pure-port)

;; net/url currently doesn't provide a `patch-pure-port'.
;;
;; Even so, if net/url at least provided its "generic"
;; `method-pure-port', the following one-liner would be all that we
;; need:
(define/contract (patch-pure-port url data [h '()])
  ((url? bytes?) ((listof string?)) . ->* . input-port?)
  (method-pure-port 'patch url data h))

;; Unfortunately, instead we need the entire rest of this file, which
;; shamelessly copies method-pure-port and its minimum supporting
;; functions from net/url. Obviously this just an interim hack until
;; patch-pure-port can be added to net/url. All I can say in my
;; defense is that this is less smelly than using rackunit's
;; require/expose to get at method-pure-port.

;; <<COPY-PASTE

(require (rename-in racket/tcp
                    [tcp-connect plain-tcp-connect]
                    [tcp-abandon-port plain-tcp-abandon-port])
         openssl)

(define current-connect-scheme (make-parameter "http"))

(define current-https-protocol (make-parameter 'sslv2-or-v3))

;; Define `tcp-connect' and `tcp-abandon-port' to fit with
;; `current-connect-scheme'
(define (tcp-connect host port)
  (cond [(equal? (current-connect-scheme) "https")
         (ssl-connect host port (current-https-protocol))]
        [else
         (plain-tcp-connect host port)]))

(define (tcp-abandon-port port)
  (cond [(ssl-port? port) (ssl-abandon-port port)]
        [else (plain-tcp-abandon-port port)]))

(struct our-url-exception exn:fail ())

(define (url-error fmt . args)
  (raise (our-url-exception
          (apply format fmt
                 (map (lambda (arg) (if (url? arg) (url->string arg) arg))
                      args))
          (current-continuation-marks))))

;; url->default-port : url -> num
(define (url->default-port url)
  (let ([scheme (url-scheme url)])
    (cond [(not scheme) 80]
          [(string=? scheme "http") 80]
          [(string=? scheme "https") 443]
          [else (url-error "URL scheme ~s not supported" scheme)])))

;; make-ports : url -> in-port x out-port
(define (make-ports url proxy)
  (let ([port-number (if proxy
                       (caddr proxy)
                       (or (url-port url) (url->default-port url)))]
        [host (if proxy (cadr proxy) (url-host url))])
    (parameterize ([current-connect-scheme (url-scheme url)])
      (tcp-connect host port-number))))

;; method-pure-port : symbol x url x list (str) -> in-port
(define (method-pure-port method url data strings)
  (let ([scheme (url-scheme url)])
    (cond [(or (string=? scheme "http") (string=? scheme "https"))
           (let ([port (http://method-impure-port
                        method url data strings)])
             (purify-http-port port))]
          ;; [(string=? scheme "file")
          ;;  (file://get-pure-port url)]
          [else (url-error "Scheme ~a unsupported" scheme)])))

;; http://metod-impure-port : symbol x url x union (str, #f) x list (str) -> in-port
(define (http://method-impure-port method url data strings)
  (let*-values
      ([(method) (case method
                   [(get) "GET"] [(post) "POST"] [(head) "HEAD"]
                   [(put) "PUT"] [(delete) "DELETE"] [(patch) "PATCH"]
                   [else (url-error "unsupported method: ~a" method)])]
       [(proxy) (assoc (url-scheme url) (current-proxy-servers))]
       [(server->client client->server) (make-ports url proxy)]
       [(access-string) (url->string
                         (if proxy
                           url
                           (make-url #f #f #f #f
                                     (url-path-absolute? url)
                                     (url-path url)
                                     (url-query url)
                                     (url-fragment url))))])
    (define (println . xs)
      (for-each (lambda (x) (display x client->server)) xs)
      (display "\r\n" client->server))
    (println method " " access-string " HTTP/1.0")
    (println "Host: " (url-host url)
             (let ([p (url-port url)]) (if p (format ":~a" p) "")))
    (when data (println "Content-Length: " (bytes-length data)))
    (for-each println strings)
    (println)
    (when data (display data client->server))
    (flush-output client->server)
    (tcp-abandon-port client->server)
    server->client))

;; purify-http-port : in-port -> in-port
;; returns a new port, closes the old one when done pumping
(define (purify-http-port in-port)
  (define-values (in-pipe out-pipe) (make-pipe))
  (thread
   (λ ()
     (define status (http-read-status in-port))
     (define chunked? (http-read-headers in-port))
     (http-pipe-data chunked? in-port out-pipe)
     (close-input-port in-port)))
  in-pipe)

(define (http-read-status ip)
  (read-line ip 'return-linefeed))

(define (http-read-headers ip)
  (define l (read-line ip 'return-linefeed))
  (when (eof-object? l)
    (error 'purify-http-port "Connection ended before headers ended"))
  (if (string=? l "")
      #f
      (if (string=? l chunked-header-line)
          (begin (http-read-headers ip)
                 #t)
          (http-read-headers ip))))

(define chunked-header-line "Transfer-Encoding: chunked")

(define (http-pipe-data chunked? ip op)
  (if chunked?
      (http-pipe-chunk ip op)
      (begin
        (copy-port ip op)
        (flush-output op)
        (close-output-port op))))

(define (http-pipe-chunk ip op)
  (define crlf-bytes (make-bytes 2))
  (let loop ([last-bytes #f])
    (define size-str (read-line ip 'return-linefeed))
    (define chunk-size (string->number size-str 16))
    (unless chunk-size
      (error 'http-pipe-chunk "Could not parse ~S as hexadecimal number" size-str))
    (define use-last-bytes?
      (and last-bytes (<= chunk-size (bytes-length last-bytes))))
    (if (zero? chunk-size)
        (begin (flush-output op)
               (close-output-port op))
        (let* ([bs (if use-last-bytes?
                       (begin
                         (read-bytes! last-bytes ip 0 chunk-size)
                         last-bytes)
                       (read-bytes chunk-size ip))]
               [crlf (read-bytes! crlf-bytes ip 0 2)])
          (write-bytes bs op 0 chunk-size)
          (loop bs)))))

;; COPY-PASTE
