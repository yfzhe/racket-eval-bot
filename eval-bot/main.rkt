#lang racket/base
(require telebot
         telebot/text
         telebot/command
         racket/match
         racket/string
         "eval.rkt")

(define token (getenv "BOT_TOKEN"))
(define port (string->number (getenv "PORT")))
(define webhook-base (getenv "WEBHOOK_BASE"))

(unless token
  (error "does not setup BOT_TOKEN env-var"))
(define bot (make-bot token))

(define (handle-update update)
  (cond
    [(ref (update : update) .message #f)
     => handle-message]))

(define (handle-message message)
  (define text (ref (message : message) .text #f))
  (define in-private-chat?
    (equal? (ref (message : message) .chat .type) "private"))
  (cond
    [(not text) (void)]
    [(parse-command text (bot-username bot))
     =>
     (match-lambda
       [(list "start" _) (start message)]
       [(list "help" _) (help message)]
       [(list "eval" code) (eval message code 'racket)]
       [(list "eval_chez" code) (eval message code 'chez)]
       [(list _ _) (bad-request message)])]
    [in-private-chat?
     (eval message text 'racket)]
    [else (void)]))

(define (start message)
  (bot-send-message bot
                    #:chat-id (ref (message : message) .chat .id)
                    #:parse-mode "MarkdownV2"
                    #:text #<<END
Welcome to *racket\-eval\-bot*\!
Type your code, and wait for the result\.
END
                    ))

(define (help message)
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

(define (bad-request message)
  (bot-send-message bot
                    #:chat-id (ref (message : message) .chat .id)
                    #:parse-mode "MarkdownV2"
                    #:text "Not supported command"))

;; mode: 'racket or 'chez
(define (eval message code mode)
  (define eval-result
    (match mode
      ['racket (eval-code code)]
      ['chez (eval-code/chez code)]))
  (bot-send-message bot
                    #:chat-id (ref (message : message) .chat .id)
                    #:reply (make-reply-params
                             #:message-id (message-id message))
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

(module+ main
  (bot-start/webhook bot handle-update
                     webhook-base port))
