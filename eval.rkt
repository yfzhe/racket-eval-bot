#lang racket/base
(require racket/sandbox
         racket/format
         xml)

(provide eval-code)

(define (eval-code code)
  (define evaluator
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-propagate-exceptions #f]
                   [sandbox-security-guard (current-security-guard)])
      (make-evaluator 'racket)))
  (define result (evaluator code))

  (define output (get-output evaluator))
  (define error (get-error-output evaluator))
  (define result*
    (if (void? result)
        ""
        (call-in-sandbox-context
         evaluator
         (lambda () (~v result)))))

  (kill-evaluator evaluator)

  (cond
    [(string=? (string-append output error result*) "")
     (xexpr->string `(del "nothing to output"))]
    [else
     (apply string-append
            (map xexpr->string
                 `((u ,output)
                   (em ,error)
                   ,result*)))]))
