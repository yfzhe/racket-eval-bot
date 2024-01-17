#lang racket/base
(require "private/schema.rkt")

(provide (all-defined-out))

(define-schema user
  (id integer?)
  (bot? boolean? "is_bot")
  (username (optional string?)))

(define-schema chat
  (id integer?)
  (type string?)
  (title (optional string?))
  (username (optional string?)))

(define-schema message
  (id integer? "message_id")
  (date integer?)
  (chat chat)
  (text (optional string?)))

(define-schema response
  (chat-id integer?)
  (text string?)
  (parse-mode (optional string?))
  (reply (optional reply-params) "reply_parameters"))

(define-schema reply-params
  (message-id integer?))

(define-schema update
  (id integer? "update_id")
  (message (optional message)))

;; api methods
(define-api bot-get-me "/getMe"
  () -> user)

(define-api bot-send-message "/sendMessage"
  response -> message)
