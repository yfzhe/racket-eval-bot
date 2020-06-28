#lang racket/base
(require (for-syntax racket/base))

(provide hash-ref*
         await await/proc)

;; hash-ref*: chainning ref nested hash keys
;; NOTE: it seems that these two does not need to be macros
(define-syntax (hash-ref* stx)
  (syntax-case stx ()
    [(_ hash key)
     #'(hash-ref hash key)]
    [(_ hash key more ...)
     #'(hash-ref* (hash-ref hash key) more ...)]))

;; hash-ref**: some kind of "optional chain"
(define-syntax (hash-ref** stx)
  (syntax-case stx ()
    [(_ hash key)
     #'(hash-ref hash key #f)]
    [(_ hash key more ...)
     #'(let ([val (hash-ref hash key #f)])
         (and val (hash-ref** val more ...)))]))

;; await: execute in another thread
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
