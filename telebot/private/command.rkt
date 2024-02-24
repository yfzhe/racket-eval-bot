#lang racket/base
(require racket/match
         "bot.rkt")

(provide command-name command-proc
         bot-add-command!
         parse-command)

;; - name: string
;; - proc: bot message string -> void
;;   TODO: want to remove these 2 arguments, `bot` and `message`,
;;     before that, we need a better abstraction on bot api methods.
(struct command (name proc))

(define (bot-add-command! bot name proc)
  (set-bot-commands! bot
                     (append (bot-commands bot)
                             (list (command name proc)))))

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
