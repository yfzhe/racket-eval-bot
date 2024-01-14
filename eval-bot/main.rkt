#lang racket/base
(require telebot
         racket/match
         racket/string
         xml
         "eval.rkt")

(define token (getenv "BOT_TOKEN"))
(define port (string->number (getenv "PORT")))
(define webhook-base (getenv "WEBHOOK_BASE"))

(unless token
  (error "does not setup BOT_TOKEN env-var"))
(define bot (make-bot token))

(define (handle-update update)
  (define message (ref (update : update) .message #f))
  (define resp (handle-message message))
  (when resp
    (bot-send-message bot resp)))

;; TODO: i need a robuster way to parse commands
(define (handle-message message)
  (define text (ref (message : message) .text #f))
  (match text
    ;; currently only handle "message" updates with `text` field
    ;; `#f` represents not to respond
    [#f #f]
    [(regexp #rx"^/start")
     (start message)]
    [(regexp #rx"^/help")
     (help message)]
    [(regexp #px"^/eval\\b(@[a-z_]+)?(.+)$" (list _ _ code))
     (eval message (string-trim code) 'racket)]
    [(regexp #px"^/eval_chez\\b(@[a-z_]+)?(.+)$" (list _ _ code))
     (eval message (string-trim code) 'chez)]
    [_
     (cond
       [(equal? (ref (message : message) .chat .type)
                "private")
        (match text
          [(regexp #rx"^/")
           (bad-request message)]
          [_ (eval message text 'racket)])]
       [else #f])]))

(define (start message)
  (make-response #:chat-id (ref (message : message) .chat .id)
                 #:parse-mode "MarkdownV2"
                 #:text #<<END
Welcome to *racket\-eval\-bot*\!
Type your code, and wait for the result\.
END
                 ))

(define (help message)
  (make-response #:chat-id (ref (message : message) .chat .id)
                 #:parse-mode "MarkdownV2"
                 #:text #<<END
/start\: start to use this bot
/eval \<code\>\: eval code
/eval\_chez \<code\>\: eval code with Chez Scheme \(inside Racket\)
/help\: show this message
END
                 ))

(define (bad-request message)
  (make-response #:chat-id (ref (message : message) .chat .id)
                 #:parse-mode "MarkdownV2"
                 #:text "Not supported command"))

;; mode: 'racket or 'chez
(define (eval message code mode)
  (define eval-result
    (match mode
      ['racket (eval-code code)]
      ['chez (eval-code/chez code)]))
  (make-response #:chat-id (ref (message : message) .chat .id)
                 #:reply (make-reply-params
                          #:message-id (message-id message))
                 #:parse-mode "HTML"
                 #:text (process-eval-results eval-result)))

(define (process-eval-results results)
  (define xexprs
    (for/list ([r (in-list results)])
      (match-define (list result output error) r)
      `(,@(if (non-empty-string? output) `((pre ,output)) '())
        ,@(if (non-empty-string? error) `((em ,error)) '())
        ,@(if (non-empty-string? result) `(,result) '()))))

  (define str (apply string-append
                     (map xexpr->string (apply append xexprs))))
  (if (non-empty-string? str)
      str
      (xexpr->string `(pre "[nothing to output]"))))

(module+ main
  (bot-start/webhook bot handle-update
                     webhook-base port))
