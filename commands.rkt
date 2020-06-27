#lang racket/base
(require racket/match
         "eval.rkt"
         "util.rkt")

(provide handle-message)

(define (handle-message message)
  (define text (hash-ref message 'text))
  (cond
    [(regexp-match #rx"^/start" text)
     (start message)]
    [(regexp-match #rx"^/help" text)
     (help message)]
    [(regexp-match #rx"^/eval (.+)$" text)
     =>
     (match-lambda
       [(list _ code)
        (eval message code)])]
    [(equal? (hash-ref* message 'chat 'type)
             "private")
     (eval message text)]))

(define (start message)
  (define chat-id (hash-ref* message 'chat 'id))
  `((chat_id . ,chat-id)
    (parse_mode . "MarkdownV2")
    (text . #<<END
Welcome to **Racket Eval Bot**\!
Type your code, and wait for the result\.
END
          )))

(define (help message)
  (define chat-id (hash-ref* message 'chat 'id))
  `((chat_id . ,chat-id)
    (parse_mode . "MarkdownV2")
    (text . #<<END
/start \- start to use this bot
/eval  \- eval code
/help  \- show this message
END
          )))

(define (eval message code)
  (define message-id (hash-ref message 'message_id))
  (define chat-id (hash-ref* message 'chat 'id))
  (define result (eval-code code))
  `((chat_id . ,chat-id)
    (parse_mode . "HTML")
    (text . ,result)
    #;(reply_to_message_id . ,message-id)))
