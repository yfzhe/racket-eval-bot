#lang racket/base
(require racket/match
         racket/sandbox
         racket/string
         racket/format
         ffi/unsafe/vm
         xml)

(provide eval-code
         eval-code/chez)

;; To support any #lang, both the s-exp and the non-sexp ones,
;; we have to read (or parse) the code by ourselves.
;;
;; One approach is to read the whole file including the #lang line,
;; and extract the module body from the `(module name lang body ...)`.
;; See: https://github.com/AlexKnauth/scribble-code-examples
;;
;; However, I choose other way here. To use `sandbox-run-submodules` to
;; load `configure-runtime` submod, it would setup `current-read-interaction`
;; (and print-related parameters, that are critical for Rhombus). Then We
;; access its value as `read-syntax` which read code in interactive mode.

(define (split-code code)
  (match (regexp-match #rx"^#lang (.+)\n(.+)" code)
    [(list _ lang body)
     (values (string->symbol lang) body)]
    [_
     (values 'racket code)]))

(define (create-evaluator lang)
  (parameterize ([sandbox-run-submodules '(configure-runtime)]
                 [sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-propagate-breaks #f]
                 [sandbox-propagate-exceptions #f]
                 [sandbox-make-code-inspector current-code-inspector]
                 [sandbox-memory-limit 64])
    (make-module-evaluator (format "#lang ~a" lang))))

(define (do-eval evaluator code [eval (lambda (stx) (evaluator stx))])
  (define read-syntax
    (call-in-sandbox-context evaluator current-read-interaction))

  (define results
    (for/list ([stx (in-port (lambda (in) (read-syntax 'repl in))
                             (open-input-string code))])
      ;; the result might be multi-values
      (define result
        (call-with-values (lambda () (eval stx)) list))

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

      (list result-string output error)))
  (kill-evaluator evaluator)
  (process-results results))

(define (process-results results)
  (define xexprs
    (for/list ([r (in-list results)])
      (match-define (list result output error) r)
      `(,@(if (non-empty-string? output) `((pre ,output)) '())
        ,@(if (non-empty-string? error) `((em ,error)) '())
        ,@(if (non-empty-string? result) `(,result) '()))))

  (define str (apply string-append
                     (map xexpr->string (apply append xexprs))))
  (if (non-empty-string? str)
      str
      (xexpr->string `(pre "[nothing to output]"))))


(define (~v/sandbox evaluator val)
  (call-in-sandbox-context evaluator
    (lambda () (~v val))))

(define (eval-code code)
  (define-values (lang body) (split-code code))
  (define evaluator (create-evaluator lang))
  (do-eval evaluator body))

(define (eval-code/chez code)
  ;; TODO: should i insert `(system-type 'vm)` check here?

  (define-values (_ stxes) (split-code code))
  (define evaluator (create-evaluator 'racket))
  (do-eval evaluator stxes
           (lambda (stx)
             (call-in-sandbox-context evaluator
               (lambda () (vm-eval (syntax->datum stx)))))))
