#lang racket/base
(require "bot.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     syntax/parse/experimental/template))

(provide define-schema
         define-api
         optional ->)

;; TODO:
;; - add field converter
;; - implement gen:custom-write
;; - provide transformer
;; - a `ref` macro: (ref v .message .id)
;; - unit struct-info and schema-info
;; - contracts?

(define json-undefined
  (let ()
    (struct undefined ())
    (undefined)))

(define-syntax optional
  (lambda (stx)
    (raise-syntax-error #f "optional should be used in define-schema" stx)))

(define-syntax ->
  (lambda (stx)
    (raise-syntax-error #f "-> should be used in define-api" stx)))

(begin-for-syntax
  (struct schema-info (fields from-jsexpr to-jsexpr))

  (define-syntax-class schema-id
    #:attributes (from-jsexpr to-jsexpr)
    (pattern id:id
             #:with info-id (format-id #'id "schema:~a" #'id)
             #:do [(define local-value (syntax-local-value #'info-id (lambda () #f)))]
             #:when (schema-info? local-value)
             #:with from-jsexpr (schema-info-from-jsexpr local-value)
             #:with to-jsexpr (schema-info-to-jsexpr local-value))
    (pattern id
             #:with from-jsexpr #'begin
             #:with to-jsexpr #'begin))

  (define-syntax-class field-type
    #:literals (optional)
    #:attributes (opt? type.from-jsexpr type.to-jsexpr)
    (pattern (optional type:schema-id) #:attr opt? #t)
    (pattern type:schema-id #:attr opt? #f))

  (define-syntax-class field
    #:attributes (name type key type.opt?
                       type.type.from-jsexpr type.type.to-jsexpr)
    (pattern (name:id type:field-type)
             #:with key #'name)
    (pattern (name:id type:field-type key*:string)
             #:with key (datum->syntax #'key* (string->symbol (syntax-e #'key*)))))

  (define-template-metafunction (make-field-failed stx)
    (syntax-parse stx
      [(_ converter-id fld:field)
       (if (attribute fld.type.opt?)
           #'json-undefined
           #'(lambda () (error 'converter-id "field \"~a\" is missed" 'fld.key)))]))

  (define-template-metafunction (make-field-kw-arg stx)
    (syntax-parse stx
      [(_ fld:field)
       #:with name #'fld.name
       #:with kw (datum->syntax #'name
                                (string->keyword (symbol->string (syntax-e #'name))))
       (if (attribute fld.type.opt?)
           #'(kw [name json-undefined])
           #'(kw name))]))

  (define-template-metafunction (make-accessor stx)
    (syntax-parse stx
      [(_ id fld:field) (format-id #'id "~a-~a" #'id #'fld.name)])))

(define-syntax (define-schema stx)
  (syntax-parse stx
    [(_ name:id fld:field ...)
     #:with schema-info-id (format-id #'name "schema:~a" #'name)
     #:with ctor-id (format-id #'name "make-~a" #'name)
     #:with jsexpr->name (format-id #'name "jsexpr->~a" #'name)
     #:with name->jsexpr (format-id #'name "~a->jsexpr" #'name)
     #'(begin
         (struct name (fld.name ...)
           ;; #:constructor-name struct-ctor-id
           #:transparent)

         (define (ctor-id (~@ . (make-field-kw-arg fld)) ...)
           (name fld.name ...))

         (define (jsexpr->name jsexpr)
           (name (fld.type.type.from-jsexpr
                  (hash-ref jsexpr 'fld.key (make-field-failed jsexpr->name fld)))
                 ...))

         (define (name->jsexpr data)
           (define jsexpr (make-hash))
           (let ([fld-v ((make-accessor name fld) data)])
             (unless (eq? fld-v json-undefined)
               (hash-set! jsexpr 'fld.key (fld.type.type.to-jsexpr fld-v)))) ...
           jsexpr)

         (define-syntax schema-info-id
           (schema-info (list #'fld ...)
                        #'jsexpr->name
                        #'name->jsexpr)))]))

(define-syntax (define-api stx)
  (syntax-parse stx
    #:literals (->)
    [(_ api-id endpoint (~optional arg:schema-id) -> ret:schema-id)
     #'(define (api-id bot (~? arg))
         (ret.from-jsexpr (bot-post bot endpoint (~? (arg.to-jsexpr arg)))))]))
