#lang racket/base
(require net/url json
         racket/function)

(provide make-bot bot?
         bot-get bot-post
         (struct-out exn:fail:bot)
         (struct-out exn:fail:bot:api) raise-bot-api-error
         bot-get-me bot-get-updates bot-send-message)

;; bot: the struct "present" bots
(struct bot (token))

(define (make-bot token) (bot token))

;; errors
(struct exn:fail:bot exn:fail ())
(struct exn:fail:bot:api exn:fail:bot (method description error-code))

(define (raise-bot-api-error method desc error-code)
  (define msg
    (format #<<END
bot: error when requesting telegram api ~a
  description: ~a
  error code: ~a
END
            method desc error-code))
  (raise
   (exn:fail:bot:api msg
                     (current-continuation-marks)
                     method
                     desc
                     error-code)))

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

(define (bot-post bot method params)
  (call/input-url
   (make-api-url bot method)
   (lambda (url)
     (post-pure-port url
                     (jsexpr->bytes (make-hash params))
                     '("Content-Type: application/json; charset=utf-8")))
   (curry handle-port method)))

(define (handle-port method port)
  (define response (read-json port))
  (if (hash-ref response 'ok)
      (hash-ref response 'result)
      (raise-bot-api-error method
                           (hash-ref response 'description)
                           (hash-ref response 'error_code))))

;; apis for each methods
(define (bot-get-me bot)
  (bot-get bot "/getMe"))

(define (bot-get-updates bot [offset 0])
  (bot-get bot
           "/getUpdates"
           `((offset . ,(number->string offset)))))

(define (bot-send-message bot message)
  (bot-post bot "/sendMessage" message))
