#lang racket/base
(require racket/match
         racket/string
         "eval.rkt"
         "util.rkt")

(provide handle-message)

;; TODO: i need a robuster way to parse commands

(define (handle-message message)
  (define text (hash-ref message 'text))
  (match text
    [(regexp #rx"^/start")
     (start message)]
    [(regexp #rx"^/help")
     (help message)]
    [(regexp #px"^/eval\\b(@[a-z_]+)?(.+)$" (list _ _ code))
     (eval message (string-trim code))]
    [(regexp #px"^/eval_chez\\b(@[a-z_]+)?(.+)$" (list _ _ code))
     (eval-chez message (string-trim code))]
    [(regexp #rx"^/")
     (bad-request message)]
    [_
     (cond
       [(equal? (hash-ref* message 'chat 'type)
                "private")
        (eval message text)]
       ;; `#f` represents not to respond
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
/eval_chez \<code\>\: eval code with Chez Scheme (inside Racket)
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

(define (eval-chez message code)
  (define message-id (hash-ref message 'message_id))
  (define chat-id (hash-ref* message 'chat 'id))
  (define result (eval-code/chez code))
  `((chat_id . ,chat-id)
    (parse_mode . "HTML")
    (text . ,result)
    (reply_to_message_id . ,message-id)))
