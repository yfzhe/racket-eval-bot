#lang racket/base
(require racket/match
         racket/string
         "eval.rkt"
         "util.rkt")

(provide handle-message)

(define (handle-message message)
  (define text (hash-ref message 'text))
  ;; TODO: handle command entity better
  (match text
    [(regexp #rx"^/start")
     (start message)]
    [(regexp #rx"^/help")
     (help message)]
    [(regexp #rx"^/eval(@[a-z_]+)?(.+)$" (list _ _ code))
     (eval message (string-trim code))]
    [(regexp #rx"^/")
     (bad-request message)]
    [_
     (cond
       [(equal? (hash-ref* message 'chat 'type)
                "private")
        (eval message text)]
       ;; `#f` represents not to make responses
       [else #f])]))

(define (start message)
  (define chat-id (hash-ref* message 'chat 'id))
  `((chat_id . ,chat-id)
    (parse_mode . "MarkdownV2")
    (text . #<<END
Welcome to *racket\-eval\-bot*\!
Type your code, and wait for the result\.
END
          )))

(define (help message)
  (define chat-id (hash-ref* message 'chat 'id))
  `((chat_id . ,chat-id)
    (parse_mode . "MarkdownV2")
    (text . #<<END
/start\: start to use this bot
/eval \<code\>\: eval code
/help\: show this message
END
          )))

(define (bad-request message)
  (define chat-id (hash-ref* message 'chat 'id))
  `((chat_id . ,chat-id)
    (parse_mode . "MarkdownV2")
    (text . "Not supported command")))

(define (eval message code)
  (define message-id (hash-ref message 'message_id))
  (define chat-id (hash-ref* message 'chat 'id))
  (define result (eval-code code))
  `((chat_id . ,chat-id)
    (parse_mode . "HTML")
    (text . ,result)
    (reply_to_message_id . ,message-id)))
