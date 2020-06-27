#lang racket/base
(require (for-syntax racket/base))

(provide hash-ref*
         await await/proc)

(define-syntax (hash-ref* stx)
  (syntax-case stx ()
    [(_ hash key)
     #'(hash-ref hash key)]
    [(_ hash key more ...)
     #'(hash-ref* (hash-ref hash key) more ...)]))

;; inspired by `async-task` and `async` from misc1/async
(define (await/proc thunk)
  ;; TODO: make a custom event otherwise a channel
  (define ch (make-channel))
  (thread
   (lambda ()
     (let ([res (thunk)])
       (channel-put ch res))))
  ch)

(define-syntax (await stx)
  (syntax-case stx ()
    [(_ body ...)
     #'(await/proc (lambda () body ...))]))
