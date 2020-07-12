#lang racket/base
(require racket/sandbox
         racket/string racket/format
         racket/match
         xml)

(provide eval-code)

(define (eval-code code)
  (define evaluator
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-propagate-exceptions #f]
                   [sandbox-security-guard (current-security-guard)])
      (make-evaluator 'racket
                      '(error-print-context-length 2))))

  ;; last expression's result might be multi-values,
  ;; so use `call-wtih-values` to box them into a list
  (define results
    (call-with-values (lambda () (evaluator code)) list))

  (define output (get-output evaluator))
  (define error (get-error-output evaluator))
  (define result*
    (match results
      [(list (? void?)) ""]
      [(list single)
       (~v/sandbox evaluator single)]
      [(list multi ...)
       (string-join (map (lambda (v) (~v/sandbox evaluator v)) multi)
                    "\n")]))

  (kill-evaluator evaluator)

  (cond
    [(string=? (string-append output error result*) "")
     (xexpr->string `(del "nothing to output"))]
    [else
     (apply string-append
            (map xexpr->string
                 `((em ,output)
                   (em ,error)
                   ,result*)))]))

(define (~v/sandbox evaluator val)
  (call-in-sandbox-context evaluator
                           (lambda () (~v val))))
