#lang racket/base
(require racket/contract/base
         racket/match
         web-server/servlet
         web-server/servlet-env
         json
         threading
         "private/bot.rkt"
         "private/command.rkt"
         "private/error.rkt"
         "private/schema.rkt"
         "api.rkt")

(provide bot?
         (contract-out (make-bot (-> string? bot?)))
         bot-add-command!
         exn:fail:bot? exn:fail:bot:api?
         (all-from-out "api.rkt")
         ref :
         bot-start/poll bot-start/webhook)

(define-api bot-get-updates "/getUpdates"
  ((offset (optional integer?))) -> (listof update))

(define-api bot-set-webhook "/setWebhook"
  ((webhook-url string? "url"))
  -> #t)

(define (bot-init! bot)
  (define me (bot-get-me bot))
  (set-bot-username! bot (ref (me : user) .username #f)))

(define (make-bot-update-handler bot handler)
  (define commands (bot-commands bot))
  (lambda (update)
    (define message (ref (update : update) .message #f))
    (cond
      [(and message
            (and~> (ref (message : message) .text #f)
                   (parse-command _ (bot-username bot))))
       =>
       (lambda (cmd-call)
         (match-define (list name arg) cmd-call)
         (define cmd (findf (lambda (cmd) (equal? (command-name cmd) name)) commands))
         (cond
           [cmd ((command-proc cmd) bot message arg)]
           [else
            (bot-send-message bot
                              #:chat-id (ref (message : message) .chat .id)
                              #:parse-mode "HTML"
                              #:text "Not supported command")]))]
      [else (handler bot update)])))

(define (bot-start/poll bot handle-update)
  (bot-init! bot)

  (define updater (make-bot-update-handler bot handle-update))

  (let loop ([offset 0] [updates '()])
    (cond
      [(null? updates)
       (loop offset (bot-get-updates bot #:offset offset))]
      [else
       (updater (car updates))
       (loop (add1 (ref (update : update) .id)) (cdr updates))])))

(define (bot-start/webhook bot handle-update webhook-base port)
  (bot-init! bot)
  (bot-set-webhook bot
                   #:webhook-url (string-append webhook-base "/webhook"))

  (define updater (make-bot-update-handler bot handle-update))

  (define (handle-webhook req)
    (thread
     (lambda ()
       (updater
        (jsexpr->update (bytes->jsexpr (request-post-data/raw req))))))

    (response/empty #:code 200))

  (serve/servlet handle-webhook
                 #:port port
                 #:listen-ip #f
                 #:servlet-path "/webhook"
                 #:command-line? #t))
