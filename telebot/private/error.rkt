#lang racket/base

(provide exn:fail:bot? raise-bot-error
         exn:fail:bot:api? raise-bot-api-error)

(struct exn:fail:bot exn:fail ())
(struct exn:fail:bot:api exn:fail:bot (method description error-code))

(define (raise-bot-error message)
  (raise
   (exn:fail:bot (format "bot: ~a" message)
                 (current-continuation-marks))))

(define (raise-bot-api-error method desc error-code)
  (define msg
    (format #<<END
bot: error when requesting telegram api ~a
  description: ~a
  error code: ~a
END
            method desc error-code))
  (raise
   (exn:fail:bot:api msg
                     (current-continuation-marks)
                     method
                     desc
                     error-code)))
