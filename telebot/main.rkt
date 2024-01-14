#lang racket/base
(require "bot.rkt"
         "error.rkt"
         "api.rkt")

(provide (all-from-out "bot.rkt")
         (all-from-out "api.rkt")
         bot-get-updates
         bot-set-webhook
         bot-start/poll)

(define (bot-get-updates bot [offset 0])
  (bot-get bot "/getUpdates"
           `((offset . ,(number->string offset)))))

(define (bot-set-webhook bot webhook-url)
  ;; TODO: getWebhookInfo and compare to the target
  (bot-post bot "/setWebhook"
            `((url . ,webhook-url))))

(define (bot-start/poll bot handle-update)
  (let loop ([offset 0] [updates '()])
    (cond
      [(null? updates)
       (loop offset (bot-get-updates bot offset))]
      [else
       (define update (car updates))
       (define resp (handle-update update))
       (bot-send-message bot resp)
       (loop (add1 (hash-ref update 'update_id)) (cdr updates))])))
