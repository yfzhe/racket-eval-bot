#lang racket/base
(require web-server/servlet
         web-server/servlet-env)

(define app-dispatch
  (dispatch-case
   [("") index-req]))

(define (index-req req)
  (response/xexpr
   '(html
     (head (title "racket-eval-bot"))
     (body (p "a racket-eval-bot on telegram")
           (p "see "
              (a ((href "https://t.me/racket_eval_bot"))
                 "@racket-eval-bot"))
           (p "source code on "
              (a ((href "https://github.com/yfzhe/racket-eval-bot"))
                 "github"))))))

(define (app req)
  (app-dispatch req))

(module+ main
  (define port
    (string->number (getenv "PORT")))

  (serve/servlet app
                 #:port port
                 #:listen-ip #f
                 #:servlet-regexp #rx""
                 #:command-line? #t))
