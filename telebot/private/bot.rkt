#lang racket/base
(require net/http-easy
         "error.rkt")

(provide bot? make-bot
         bot-post)

(struct bot (token))

(define (make-bot token)
  (bot token))

(define *tg-api-base* "https://api.telegram.org/bot")

(define (make-api-url bot method)
  (string-append *tg-api-base*
                 (bot-token bot)
                 method))

(define (bot-post bot method [params (make-hash)])
  (define resp
    (post (make-api-url bot method) #:json params))
  (define json (response-json resp))
  (if (hash-ref json 'ok)
      (hash-ref json 'result)
      (raise-bot-api-error method
                           (hash-ref json 'description)
                           (hash-ref json 'error_code))))
