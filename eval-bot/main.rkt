#lang racket/base
(require (only-in web-server/servlet
                  response/empty request-post-data/raw dispatch-case)
         web-server/servlet-env
         json
         telebot
         racket/match
         racket/string
         "eval.rkt")

(define token (getenv "BOT_TOKEN"))
(define port (string->number (getenv "PORT")))
(define webhook-url (getenv "WEBHOOK_URL"))

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
     (eval message (string-trim code))]
    [(regexp #px"^/eval_chez\\b(@[a-z_]+)?(.+)$" (list _ _ code))
     (eval-chez message (string-trim code))]
    [_
     (cond
       [(equal? (ref (message : message) .chat .type)
                "private")
        (match text
          [(regexp #rx"^/")
           (bad-request message)]
          [_ (eval message text)])]
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

(define (eval message code)
  (define result (eval-code code))
  (make-response #:chat-id (ref (message : message) .chat .id)
                 #:reply (make-reply-params
                          #:message-id (ref (message : message) .id))
                 #:parse-mode "HTML"
                 #:text result))

(define (eval-chez message code)
  (define result (eval-code/chez code))
  (make-response #:chat-id (ref (message : message) .chat .id)
                 #:reply (make-reply-params
                          #:message-id (ref (message : message) .id))
                 #:parse-mode "HTML"
                 #:text result))

(module+ main
  (bot-set-webhook bot webhook-url)

  (define (handle-webhook req)
    (thread
     (lambda ()
       (handle-update
        (jsexpr->update (bytes->jsexpr (request-post-data/raw req))))))

    (response/empty #:code 200))

  (define app
    (dispatch-case
     [("webhook") #:method "post" handle-webhook]))

  (serve/servlet app
                 #:port port
                 #:listen-ip #f
                 #:servlet-regexp #rx""
                 #:command-line? #t))
