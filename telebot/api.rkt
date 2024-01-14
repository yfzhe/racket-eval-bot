#lang racket/base
(require "schema.rkt")

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

(define-schema reply-params
  (message-id integer? "message_id"))

(define-schema response
  (chat-id integer? "chat_id")
  (text string?)
  (parse-mode (optional string?) "parse_mode")
  (reply (optional reply-params) "reply_parameters"))

(define-schema update
  (id integer? "update_id")
  (message (optional message)))

;; api methods
(define-api bot-get-me "/getMe"
  -> user)

(define-api bot-send-message "/sendMessage"
  response -> message)
