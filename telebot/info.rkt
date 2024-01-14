#lang info

(define collection "telebot")

(define deps '("base"
               "http-easy-lib"
               "threading-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))

;; (define scribblings '(("scribblings/telebot.scrbl" ())))

(define pkg-desc "Telegram bot API")
(define version "0.2")

(define pkg-authors '(yfzhe))
(define license '(Apache-2.0 OR MIT))
