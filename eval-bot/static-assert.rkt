#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide static-assert
         (for-syntax (all-from-out racket/base)))

;; (static-assert condition)
;; (static-assert condition message)
;; check condition in compile-time
(define-syntax (static-assert stx)
  (syntax-parse stx
    [(_ condition (~optional message:string))
     #'(define-syntaxes ()
         (begin
           (unless condition
             (raise-syntax-error 'static-assert
                                 (~? message "failed")
                                 (syntax condition)))
           (values)))]))
