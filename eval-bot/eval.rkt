#lang racket/base
(require racket/sandbox
         racket/string racket/format
         racket/match
         xml)

(provide eval-code)

;; split-code:
;;   detect the lang from code, and extract the body part
(define (split-code code)
  (match (regexp-match #rx"^#lang (.+)\n(.+)" code)
    [(list _ lang body)
     (values (string->symbol lang) body)]
    [_
     (values 'racket code)]))

(define (create-evaluator lang)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-propagate-breaks #f]
                 [sandbox-propagate-exceptions #f]
                 [sandbox-memory-limit 64])
    (make-evaluator lang)))

(define (eval-code code)
  (define-values (lang body) (split-code code))
  (define evaluator (create-evaluator lang))

  (call-in-sandbox-context evaluator
                           (lambda () (error-print-context-length 2)))

  ;; the result of the last expression might be multi-values,
  ;; use `call-with-values` to collect them into a list.
  (define results
    (call-with-values (lambda () (evaluator body)) list))

  (define output (get-output evaluator))
  (define error (get-error-output evaluator))

  (define result-string
    (match results
      [(list (? void?)) ""]
      [(list single)
       (~v/sandbox evaluator single)]
      [(list multi ...)
       (string-join (map (lambda (v) (~v/sandbox evaluator v)) multi)
                    "\n")]))

  (kill-evaluator evaluator)

  (cond
    [(andmap (lambda (s) (equal? s ""))
             (list output error result-string))
     (xexpr->string `(pre "[nothing to output]"))]
    [else
     (apply string-append
            (map xexpr->string
                 `((pre ,output)
                   (em ,error)
                   ,result-string)))]))

(define (~v/sandbox evaluator val)
  (call-in-sandbox-context evaluator
                           (lambda () (~v val))))
