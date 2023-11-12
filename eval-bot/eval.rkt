#lang racket/base
(require racket/match
         racket/sandbox
         racket/string
         racket/format
         racket/port
         ffi/unsafe/vm
         xml)

(provide eval-code
         eval-code/chez)

;; split-code:
;;   detect the lang from code, and extract the body part
(define (split-code code)
  (match (regexp-match #rx"^#lang (.+)\n(.+)" code)
    [(list _ lang body)
     (values (string->symbol lang) body)]
    [_
     (values 'racket code)]))

(define (create-evaluator lang [requires '()])
  (define evaluator
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-propagate-breaks #f]
                   [sandbox-propagate-exceptions #f]
                   [sandbox-memory-limit 64])
      (make-evaluator lang #:requires requires)))
  (call-in-sandbox-context evaluator
    (lambda () (error-print-context-length 2)))
  evaluator)

(define (base-eval evaluator thunk)
  ;; the result of the last expression might be multi-values,
  ;; use `call-with-values` to collect them into a list.
  (define result (call-with-values thunk list))

  (define output (get-output evaluator))
  (define error (get-error-output evaluator))

  (define result-string
    (match result
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

(define (eval-code code)
  (define-values (lang body) (split-code code))
  (define evaluator (create-evaluator lang))
  (base-eval evaluator (lambda () (evaluator body))))

(define (eval-code/chez code)
  ;; TODO: should i insert `(system-type 'vm)` check here?

  (define sexp
    (let ([in (open-input-string code)])
      (port-count-lines! in)
      (define subexprs (port->list read in))
      (cond
        [(= (length subexprs) 1) (car subexprs)]
        [else (cons 'begin subexprs)])))

  (define evaluator (create-evaluator 'racket))
  (base-eval evaluator (lambda ()
                         (call-in-sandbox-context evaluator
                           (lambda () (vm-eval sexp))))))
