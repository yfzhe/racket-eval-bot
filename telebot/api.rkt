#lang racket/base
(require "private/schema.rkt"
         (for-syntax racket/base))

(define-syntax (define-schema/provide stx)
  (syntax-case stx ()
    [(_ id body ...)
     #'(begin
         (provide (schema-out id))
         (define-schema id body ...))]))

(define-syntax (define-api/provide stx)
  (syntax-case stx ()
    [(_ id body ...)
     #'(begin
         (provide id)
         (define-api id body ...))]))

(define-schema/provide user
  (id integer?)
  (bot? boolean? "is_bot")
  (username (optional string?)))

(define-schema/provide chat
  (id integer?)
  (type string?)
  (title (optional string?))
  (username (optional string?)))

(define-schema/provide message
  (id integer? "message_id")
  (date integer?)
  (chat chat)
  (text (optional string?)))

(define-schema/provide reply-params
  (message-id integer?))

(define-schema/provide update
  (id integer? "update_id")
  (message (optional message)))

;; api methods
(define-api/provide bot-get-me "/getMe"
  () -> user)

(define-api/provide bot-send-message "/sendMessage"
  ((chat-id integer?)
   (text string?)
   (parse-mode (optional string?))
   (reply (optional reply-params) "reply_parameters"))
  -> message)
