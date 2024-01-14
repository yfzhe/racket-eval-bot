#lang racket/base
(require "private/bot.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/string
                     syntax/parse/experimental/template))

(provide define-schema
         define-api
         ref
         optional : ->)

;; TODO:
;; - add field converter
;; - implement gen:custom-write
;; - provide transformer
;; - unit struct-info and schema-info
;; - contracts?

(define json-undefined
  (let ()
    (struct undefined ())
    (undefined)))

(define-syntax optional
  (lambda (stx)
    (raise-syntax-error #f "optional should be used in define-schema" stx)))

(define-syntax :
  (lambda (stx)
    (raise-syntax-error #f ": should be used in ref" stx)))

(define-syntax ->
  (lambda (stx)
    (raise-syntax-error #f "-> should be used in define-api" stx)))

(begin-for-syntax
  (struct schema-info (struct-id fields from-jsexpr to-jsexpr))

  (define-syntax-class schema-id
    #:attributes (struct-id fields from-jsexpr to-jsexpr)
    (pattern id:id
             #:with info-id (format-id #'id "schema:~a" #'id)
             #:do [(define local-value (syntax-local-value #'info-id (lambda () #f)))]
             #:when (schema-info? local-value)
             #:with struct-id (schema-info-struct-id local-value)
             #:attr fields (schema-info-fields local-value)
             #:with from-jsexpr (schema-info-from-jsexpr local-value)
             #:with to-jsexpr (schema-info-to-jsexpr local-value))
    (pattern id
             #:attr struct-id #f
             #:attr fields '()
             #:with from-jsexpr #'begin
             #:with to-jsexpr #'begin))

  (define-syntax-class field-type
    #:literals (optional)
    #:attributes (opt? type type.from-jsexpr type.to-jsexpr)
    (pattern (optional type:schema-id) #:attr opt? #t)
    (pattern type:schema-id #:attr opt? #f))

  (define-syntax-class field
    #:attributes (name type key type.opt?
                       type.type type.type.from-jsexpr type.type.to-jsexpr)
    (pattern (name:id type:field-type)
             #:with key #'name)
    (pattern (name:id type:field-type key*:string)
             #:with key (datum->syntax #'key* (string->symbol (syntax-e #'key*)))))

  (define-syntax-class ref-key
    #:attributes (trimed)
    (pattern id:id
             #:do [(define key/str (symbol->string (syntax-e #'id)))]
             #:fail-unless (string-prefix? key/str ".") "key should start with \".\""
             #:with trimed (datum->syntax #'id (string->symbol (substring key/str 1)))))

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
           (schema-info #'name
                        (list #'fld ...)
                        #'jsexpr->name
                        #'name->jsexpr)))]))

(define-syntax (ref stx)
  (syntax-parse stx
    #:literals (:)
    [(_ (value : _)) #'value]
    [(_ (value : schema:schema-id) key:ref-key more ...)
     #:with (field-name field-schema)
     (let loop ([fields (attribute schema.fields)])
       (cond
         [(null? fields)
          (raise-syntax-error 'ref (format "schema ~a don't have the field ~a" (syntax-e #'schema) (syntax-e #'key.trimed))
                              #f #'key)]
         [else
          (syntax-parse (car fields)
            [fld:field
             #:when (equal? (syntax-e #'fld.name)
                            (syntax-e #'key.trimed))
             #'(fld.name fld.type.type)]
            [_ (loop (cdr fields))])]))
     #:with struct-id #'schema.struct-id
     #:with accessor (format-id #'struct-id "~a-~a" #'struct-id #'key.trimed)
     #'(let ([x (accessor value)])
         (ref (x : field-schema) more ...))]
    [(_ (value : _) failed-value)
     #'(if (eq? value json-undefined) failed-value value)]))

(define-syntax (define-api stx)
  (syntax-parse stx
    #:literals (->)
    [(_ api-id endpoint (~optional arg:schema-id) -> ret:schema-id)
     #'(define (api-id bot (~? arg))
         (ret.from-jsexpr (bot-post bot endpoint (~? (arg.to-jsexpr arg)))))]))
