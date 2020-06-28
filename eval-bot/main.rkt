#lang racket/base
(require racket/list
         "bot.rkt"
         "commands.rkt")

(define token (getenv "BOT_TOKEN"))
(unless token
  (error "does not setup BOT_TOKEN env-var"))
(define bot (make-bot token))

(define (handle-update update)
  (define message (hash-ref update 'message #f))
  ;; currently only handle "message" updates with `text` field
  (when (and message (hash-ref message 'text #f))
    (with-handlers ([exn:fail:bot:api?
                     (lambda (e)
                       (displayln (exn:fail:bot:api->string e)))])
      (define res (handle-message message))
      (when res
        (bot-send-message bot res)))))

(define *interval* 0.1)

(define (loop offset)
  (define updates (bot-get-updates bot offset))
  (cond
    [(null? updates)
     (sleep *interval*)
     (loop offset)]
    [else
     (for-each handle-update updates)

     (sleep *interval*)

     (define last-update-id
       (hash-ref (last updates) 'update_id))
     (loop (add1 last-update-id))]))

(module+ main
  (require "webpage.rkt")

  ;; heroku need us use a port, so make a web page
  (when (getenv "PORT")
    (void (thread run-webpage)))

  (loop 0))