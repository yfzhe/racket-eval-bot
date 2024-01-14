#lang racket/base
(require net/url json
         racket/function
         "error.rkt")

(provide make-bot bot?
         bot-get bot-post)

(struct bot (token webhook-url))

(define (make-bot token #:webhook-url [webhook-url #f])
  (bot token webhook-url))

;; telegram api url
(define *tg-api-base* "https://api.telegram.org/bot")

(define (make-api-url bot method)
  (string->url
   (string-append *tg-api-base*
                  (bot-token bot)
                  method)))

(define (make-api-url/query bot method query)
  (struct-copy url
               (make-api-url bot method)
               [query query]))

;; two low-level api for communicating with telegram
;; both take a `bot`, a method string, and an alist for params (or query)
(define (bot-get bot method [params '()])
  (call/input-url
   (make-api-url/query bot method params)
   get-pure-port
   (curry handle-port method)))

(define (bot-post bot method [params '()])
  (call/input-url
   (make-api-url bot method)
   (lambda (url)
     (post-pure-port url
                     (jsexpr->bytes
                      (if (hash? params) params (make-hash params)))
                     '("Content-Type: application/json; charset=utf-8")))
   (curry handle-port method)))

(define (handle-port method port)
  (define response (read-json port))
  (if (hash-ref response 'ok)
      (hash-ref response 'result)
      (raise-bot-api-error method
                           (hash-ref response 'description)
                           (hash-ref response 'error_code))))
