#lang racket/base
(require web-server/servlet
         web-server/servlet-env
         json
         telebot
         "commands.rkt")

(define token (getenv "BOT_TOKEN"))
(define port (string->number (getenv "PORT")))
(define webhook-url (getenv "WEBHOOK_URL"))

(unless token
  (error "does not setup BOT_TOKEN env-var"))
(define bot (make-bot token))

(define app
  (dispatch-case
   [("webhook") #:method "post" handle-webhook]))

(define (handle-webhook req)
  (thread
   (lambda ()
     (handle-update
      (bytes->jsexpr (request-post-data/raw req)))))

  (response/empty #:code 200))

(define (handle-update update)
  (define message (hash-ref update 'message #f))
  ;; currently only handle "message" updates with `text` field
  (when (and message (hash-ref message 'text #f))
    (define res (handle-message message))
    (when res
      (bot-send-message bot res))))

(module+ main
  (bot-set-webhook bot webhook-url)

  (serve/servlet app
                 #:port port
                 #:listen-ip #f
                 #:servlet-regexp #rx""
                 #:command-line? #t))
