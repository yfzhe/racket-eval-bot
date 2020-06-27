#lang racket/base
(require web-server/servlet
         web-server/servlet-env)

(provide run-webpage)

(define app-dispatch
  (dispatch-case
   [("") index-req]))

(define (index-req req)
  (response/xexpr
   '(html
     (body "Hello, world"))))

(define (app req)
  (app-dispatch req))

(define (run-webpage)
  (define port
    (string->number (getenv "PORT")))
  (serve/servlet app
                 #:port port
                 #:listen-ip #f
                 #:servlet-regexp #rx""
                 #:command-line? #t))
