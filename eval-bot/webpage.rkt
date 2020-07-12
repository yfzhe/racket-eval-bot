#lang racket/base
(require web-server/servlet
         web-server/servlet-env)

(define app-dispatch
  (dispatch-case
   [("") index-req]))

(define (index-req req)
  (response/xexpr
   '(html
     (head (title "racket-eval-bot")
           (meta ((name "viewport")
                  (content "width=device-width, initial-scale=1"))))
     (body (p "A telegram bot to eval racket code")
           (p "See "
              (a ((href "https://t.me/racket_eval_bot"))
                 "@racket_eval_bot"))
           (p "Source code on "
              (a ((href "https://github.com/yfzhe/racket-eval-bot"))
                 "GitHub"))))))

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
