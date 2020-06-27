#lang racket/base
(require (for-syntax racket/base))

(provide hash-ref*)

(define-syntax (hash-ref* stx)
  (syntax-case stx ()
    [(_ hash key)
     #'(hash-ref hash key)]
    [(_ hash key more ...)
     #'(hash-ref* (hash-ref hash key) more ...)]))
