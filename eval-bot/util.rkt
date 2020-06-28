#lang racket/base
(require (for-syntax racket/base))

(provide hash-ref* hash-ref**
         async)

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

;; async: execute in another thread
;; inspired by misc1/async
(define-syntax (async stx)
  (syntax-case stx ()
    [(_ body ...)
     #'(let ()
         (define ch (make-channel))
         (define exn-ch (make-channel))
         (thread
          (lambda ()
            (with-handlers ([exn?
                             (lambda (e)
                               (channel-put exn-ch e))])
              (define res (begin body ...))
              (channel-put ch res))))
         (choice-evt (wrap-evt ch values)
                     (wrap-evt exn-ch raise)))]))
