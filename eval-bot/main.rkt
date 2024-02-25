#lang racket/base
(require telebot
         telebot/text
         racket/match
         racket/string
         "eval.rkt")

(define (start bot message _arg)
  (bot-send-message bot
                    #:chat-id (ref (message : message) .chat .id)
                    #:parse-mode "MarkdownV2"
                    #:text #<<END
Welcome to *racket\-eval\-bot*\!
Type your code, and wait for the result\.
END
                    ))

(define (help bot message _arg)
  (bot-send-message bot
                    #:chat-id (ref (message : message) .chat .id)
                    #:parse-mode "MarkdownV2"
                    #:text #<<END
/start\: start to use this bot
/eval \<code\>\: eval code
/eval\_chez \<code\>\: eval code with Chez Scheme \(inside Racket\)
/help\: show this message
END
                    ))

;; mode: 'racket or 'chez
(define ((eval mode) bot message code)
  (define eval-result
    (match mode
      ['racket (eval-code code)]
      ['chez (eval-code/chez code)]))

  (bot-send-message bot
                    #:chat-id (ref (message : message) .chat .id)
                    #:reply (make-reply-params
                             #:message-id (ref (message : message) .id))
                    #:parse-mode "HTML"
                    #:text (process-eval-results eval-result)))

(define (process-eval-results results)
  (define xexprs
    (for/list ([r (in-list results)])
      (match-define (list result output error) r)
      `(@ (code ,output)
          (em ,error)
          ,result)))

  (define str (xexpr*->string `(@ . ,xexprs)))
  (if (non-empty-string? str)
      str
      (xexpr*->string `(code "[nothing to output]"))))

(define (handle-update bot update)
  (cond
    [(ref (update : update) .message #f)
     =>
     (lambda (message)
       (define text (ref (message : message) .text #f))
       (define in-private-chat?
         (equal? (ref (message : message) .chat .type) "private"))
       (cond
         [(and in-private-chat? text)
          ((eval 'racket) bot message text)]
         [else (void)]))]))

(module+ main
  (define token (getenv "BOT_TOKEN"))
  (define port (getenv "PORT"))
  (define webhook-base (getenv "WEBHOOK_BASE"))

  (unless token
    (error "does not setup BOT_TOKEN env-var"))
  (define bot (make-bot token))

  (bot-add-command! bot "start" start)
  (bot-add-command! bot "help" help)
  (bot-add-command! bot "eval" (eval 'racket))
  (bot-add-command! bot "eval_chez" (eval 'chez))

  (bot-start/webhook bot handle-update
                     webhook-base (string->number port)))
