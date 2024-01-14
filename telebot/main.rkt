#lang racket/base
(require (only-in web-server/servlet
                  response/empty request-post-data/raw)
         web-server/servlet-env
         json
         "private/bot.rkt"
         "private/error.rkt"
         "schema.rkt"
         "api.rkt")

(provide bot? make-bot
         exn:fail:bot? exn:fail:bot:api?
         (all-from-out "api.rkt")
         ref :
         bot-start/poll bot-start/webhook)

(define (bot-get-updates bot [offset 0])
  (bot-post bot "/getUpdates"
            (hasheq 'offset (number->string offset))))

(define (bot-set-webhook bot webhook-url)
  ;; TODO: getWebhookInfo and compare to the target
  (bot-post bot "/setWebhook"
            (hasheq 'url webhook-url)))

(define (bot-start/poll bot handle-update)
  (let loop ([offset 0] [updates '()])
    (cond
      [(null? updates)
       (loop offset (bot-get-updates bot offset))]
      [else
       (define update (jsexpr->update (car updates)))
       (define resp (handle-update update))
       (bot-send-message bot resp)
       (loop (add1 (update-id update)) (cdr updates))])))

(define (bot-start/webhook bot handle-update webhook-base port)
  (define webhook-url
    (string-append webhook-base "/webhook"))

  (bot-set-webhook bot webhook-url)

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
