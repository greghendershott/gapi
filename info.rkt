#lang setup/infotab

(define name "gapi")
(define categories '(net))
(define blurb
  '("Support for 40+ Google web services via the Google"
    "API Discovery service. Choose functions generated"
    "at compile-time, or, dynamic service exploration"
    "at run-time."))
(define homepage "https://github.com/greghendershott/gapi")

(define release-notes
  '((p "Initial version. If PLaneT allowed 0.x version numbers, I'd probably call this 0.8.")))
(define version "2012-10-16")
(define can-be-loaded-with 'all)

(define primary-file '("main.rkt"))
(define scribblings '(("manual.scrbl" (multi-page))))
(define compile-omit-paths '("examples"))

(define required-core-version "5.3")
(define repositories '("4.x"))
