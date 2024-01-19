#lang racket/base
(require (only-in web-server/servlet
                  response/empty request-post-data/raw)
         web-server/servlet-env
         json
         "private/bot.rkt"
         "private/error.rkt"
         "private/schema.rkt"
         "api.rkt")

(provide bot? make-bot
         exn:fail:bot? exn:fail:bot:api?
         (all-from-out "api.rkt")
         ref :
         bot-start/poll bot-start/webhook)

(define-api bot-get-updates "/getUpdates"
  ((offset (optional integer?))) -> (listof update))

(define-api bot-set-webhook "/setWebhook"
  ((webhook-url string? "url"))
  -> true?)

(define (bot-start/poll bot handle-update)
  (let loop ([offset 0] [updates '()])
    (cond
      [(null? updates)
       (loop offset (bot-get-updates bot #:offset offset))]
      [else
       (define update (car updates))
       (define resp (handle-update update))
       (bot-send-message bot resp)
       (loop (add1 (ref (update : update) .id)) (cdr updates))])))

(define (bot-start/webhook bot handle-update webhook-base port)
  (bot-set-webhook bot
                   #:webhook-url (string-append webhook-base "/webhook"))

  (define (handle-webhook req)
    (thread
     (lambda ()
       (handle-update
        (jsexpr->update (bytes->jsexpr (request-post-data/raw req))))))

    (response/empty #:code 200))

  (serve/servlet handle-webhook
                 #:port port
                 #:listen-ip #f
                 #:servlet-path "/webhook"
                 #:command-line? #t))
