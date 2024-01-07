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
   [("") handle-webpage]
   [("webhook") #:method "post" handle-webhook]))

(define (handle-webpage req)
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
