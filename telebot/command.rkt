#lang racket/base
(require racket/match)

(provide parse-command)

;; TODO:
;; - compare bot's username
;; - how to process command arguments?

(define rx #px"^/([a-zA-Z_]+)(@[a-zA-Z_]+)?\\s?(.*)")

(define (parse-command text)
  (match (regexp-match rx text)
    [(list _ command username args)
     (list command args)]
    [#f #f]))

(module+ test
  (require rackunit)

  (check-equal? (parse-command "hello world") #f)
  (check-equal? (parse-command "/start")
                '("start" ""))
  (check-equal? (parse-command "/help@racket_eval_bot")
                '("help" ""))
  (check-equal? (parse-command "/eval (+ 1 2)")
                '("eval" "(+ 1 2)"))
  (check-equal? (parse-command "/eval_chez@racket_eval_bot\n(+ 1 2)")
                '("eval_chez" "(+ 1 2)")))
