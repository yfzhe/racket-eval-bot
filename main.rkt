#lang racket/base
(require "bot.rkt"
         "eval.rkt"
         "util.rkt"
         "webpage.rkt")

(define token (getenv "BOT_TOKEN"))
(define bot
  (if token
      (make-bot token)
      (error "does not setup BOT_TOKEN env-var")))

(define (eval-message message)
  (define message-id (hash-ref message 'message_id))
  (define chat-id (hash-ref* message 'chat 'id))
  (define text (hash-ref message 'text))
  (define result (eval-code text))
  (bot-send-message bot
                    `((chat_id . ,chat-id)
                      (text . ,result)
                      (parse_mode . "HTML")
                      #;(reply_to_message_id . ,message-id))))

(define *interval* 0.1)

(define (loop offset)
  (define updates (bot-get-updates bot offset))
  (cond
    [(null? updates)
     (sleep *interval*)
     (loop offset)]
    [else
     (define update (car updates))
     (with-handlers ([exn:fail:bot:api?
                      (lambda (e)
                        (printf "Request error at ~a: ~a\n"
                                (exn:fail:bot:api-method e)
                                (exn:fail:bot:api-description e)))])
       (define message (hash-ref update 'message))
       (eval-message message))
     (sleep *interval*)
     (loop (add1 (hash-ref update 'update_id)))]))

(module+ main
  (require "webpage.rkt")

  ;; heroku need us use a port, so make a web page
  (when (getenv "PORT")
    (void (thread run-webpage)))

  (loop 0))
