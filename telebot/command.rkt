#lang racket/base
(require racket/match)

(provide parse-command)

(define rx #px"^/([a-z0-9_]+)@?([a-zA-Z0-9_]+)?\\s?(.*)")

(define (parse-command text bot-username)
  (match (regexp-match rx text)
    [(list _ command (or #f (== bot-username)) args)
     (list command args)]
    [_ #f]))

(module+ test
  (require rackunit)

  (check-equal? (parse-command "hello world" "bot")
                #f)
  (check-equal? (parse-command "/start" "bot")
                '("start" ""))
  (check-equal? (parse-command "/help@bot" "bot")
                '("help" ""))
  (check-equal? (parse-command "/help@cot" "bot")
                #f)
  (check-equal? (parse-command "/eval (+ 1 2)" "bot")
                '("eval" "(+ 1 2)"))
  (check-equal? (parse-command "/eval_chez@bot\n(+ 1 2)" "bot")
                '("eval_chez" "(+ 1 2)"))
  (check-equal? (parse-command "/plus12@answer42" "answer52")
                #f))
