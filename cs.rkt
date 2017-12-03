#lang racket
;;  Copyright (C) 2017  Zaoqi

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.

;;  You should have received a copy of the GNU Affero General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
(provide include/quote/list run-xs run run-sexps run-sexp sexp-> ->sexp)
(define-syntax-rule (include/quote/list f)
  (include/reader
   f
   (λ (source-name in)
     (define (do)
       (let ([x (read in)])
         (if (eof-object? x)
             '()
             (cons x (do)))))
     (let ([x (do)])
       (if (null? x)
           eof
           (datum->syntax #f (list 'quote x)))))))

(define (run-xs xs) (unQUOTE (EVAL gms genv (cons "begin" (QUOTE xs)))))
(define (run x) (run-xs (list x)))
(define (run-sexps xs) (->sexp (run-xs (map sexp-> xs))))
(define (run-sexp x) (run-sexps (list x)))
(define QUOTE
  (match-lambda
    [(? pair? x)
     (match x
       [(list a ... "." d) `(,@(map QUOTE a) . ,(QUOTE d))]
       [(list x ...) (map QUOTE x)])]
    [x x]))
(define unQUOTE
  (match-lambda
    [(? pair? x)
     (match x
       [(list x ...) (map unQUOTE x)]
       [(list-rest a ... d) `(,@(map unQUOTE a) "." ,(unQUOTE d))])]
    [x x]))
(define sexp->
  (match-lambda
    [(? symbol? x) (symbol->string x)]
    [(? string? x) (list "quote" x)]
    [(? number? x) (exact->inexact x)]
    [(? pair? x)
     (match x
       [(list x ...) (map sexp-> x)]
       [(list-rest a ... d) `(,@(map sexp-> a) "." ,(sexp-> d))])]
    [(? keyword? x) (string-append "#:" (keyword->string x))]
    [x x]))
(define ->sexp
  (match-lambda
    [(? string? x) (string->symbol x)]
    [(? number? x) (inexact->exact x)]
    [(? pair? x)
     (match x
       [(list a ... "." d) `(,@(map ->sexp a) . ,(->sexp d))]
       [(list x ...) (map ->sexp x)])]
    [x x]))
(define (macroexpand ms x)
  (let ([b (and (pair? x) (hash-ref ms (car x) #f))])
    (if b
        (macroexpand ms (apply (force b) (cdr x)))
        x)))
(define (EVAL ms env x)
  (match (macroexpand ms x)
    ["__PRELUDE_END__" (set! gms ms) (set! genv env)]
    [`("if" ,b ,x ,y)
     (if (EVAL ms env b)
         (EVAL ms env x)
         (EVAL ms env y))]
    [`("quote" ,x) x]
    [`("λ" ,args ,@b) (λ xs (BEGIN ms (unify env args xs) b))]
    [`("begin" ,@b) (BEGIN ms env b)]
    [`("cond" ,@b) (COND ms env b)]
    [(and x (or (? number?) (? boolean?))) x]
    [(? string? x) (force (hash-ref env x))]
    ['() '()]
    [(list f xs ...) (apply (EVAL ms env f) (map (λ (x) (EVAL ms env x)) xs))]))
(define (unify env args xs)
  (match args
    [(? string? s) (hash-set env s xs)]
    ['() (match xs ['() env])]
    [(cons (? string? s) args) (unify (hash-set env s (car xs)) args (cdr xs))]))
(define (preBEGIN ms xs)
  (if (null? xs)
      '()
      (let ([x (macroexpand ms (car xs))] [xs (cdr xs)])
        (match x
          [`("begin" ,@b) (preBEGIN ms (append b xs))]
          [x (cons x (preBEGIN ms xs))]))))
(define DEFINE
  (match-lambda
    [`("define" ,(? string? f) ,x) (cons f x)]
    [`("define" ,(cons f args) ,@x) (DEFINE `("define" ,f ("λ" ,args ,@x)))]))
(define defmacro
  (match-lambda
    [`("defmacro" ,(cons (? string? f) args) ,@x) (cons f `("λ" ,args ,@x))]
    [`("defmacro" ,(? string? f) ,x) (cons f x)]))
(define (hash-append hash ps)
  (foldl (λ (p h) (hash-set h (car p) (cdr p))) hash ps))
(define (BEGIN ms env xs)
  (let ([xs (preBEGIN ms xs)])
    (let-values ([(macs xs) (partition (match-lambda [`("defmacro" ,s ,@v) #t] [_ #f]) xs)])
      (let-values ([(defs xs) (partition (match-lambda [`("define" ,s ,@v) #t] [_ #f]) xs)])
        (let ([defs (map DEFINE defs)] [macs (map defmacro macs)])
          (if (null? macs)
              (letrec
                  ([ndefs
                    (map (λ (x)
                           (cons (car x) (delay (EVAL ms newenv (cdr x))))) defs)]
                   [newenv (hash-append env ndefs)])
                (last (map (λ (x) (EVAL ms newenv x)) xs)))
              (let ([xs (append (map (λ (x) `("define" ,(car x) ,(cdr x))) defs) xs)])
                (letrec
                    ([ndefs
                      (map (λ (x)
                             (cons (car x) (delay (EVAL newms newenv (cdr x))))) defs)]
                     [newenv (hash-append env ndefs)]
                     [nmacs
                      (map (λ (x)
                             (cons (car x) (delay (EVAL newms newenv (cdr x))))) macs)]
                     [newms (hash-append ms nmacs)])
                  (BEGIN newms newenv xs)))))))))
(define (COND ms env xs)
  (match (car xs)
    [`["else" ,@x] (match (cdr xs) ['() (BEGIN ms env x)])]
    [`[,c ,@x] (if (EVAL ms env c)
                   (BEGIN ms env x)
                   (COND ms env (cdr xs)))]))
(define csprelude
  '((defmacro (define-record-type name
                constructor pred . fields)
      (define (deffs n ref rset! fs)
        (if (null? fs)
            '()
            (let ([f (car fs)] [fs (cdr fs)])
              (if (null? (cdr (cdr f)))
                  (cons
                   `(define (,(second f) x) (,ref x ,n))
                   (deffs (+ n 1) ref rset! fs))
                  (append
                   `((define (,(second f) x) (,ref x ,n))
                     (define (,(third f) x v) (,rset! x ,n v)))
                   (deffs (+ n 1) ref rset! fs))))))
      (let ([s (genstr!)] [make (genstr!)] [ref (genstr!)] [rset! (genstr!)])
        `(begin
           (define ,s (__MK_REC__ (quote ,name) ,(length fields)))
           (define ,make (first ,s))
           (define ,pred (second ,s))
           (define ,ref (third ,s))
           (define ,rset! (fourth ,s))
           (define ,constructor (,make ,@(map first fields)))
           ,@(deffs 0 ref rset! fields)
           )))))
(define chars-num (set #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))
(define chars-abc
  (set #\q #\w #\e #\r #\t #\y #\u #\i #\o #\p #\a #\s #\d #\f #\g #\h #\j #\k #\l #\z #\x #\c #\v #\b #\n #\m
       #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P #\A #\S #\D #\F #\G #\H #\J #\K #\L #\Z #\X #\C #\V #\B #\N #\M))
(define chars (set-union chars-num chars-abc))
(define chars_ (set-add chars #\_))
(define (id str)
  (let ([xs (string->list str)])
    (if (and (andmap (λ (c) (set-member? chars_ c)) xs) (set-member? chars (car xs)))
        str
        (apply string-append
               (append
                (list "z")
                (map (λ (c)
                       (if (set-member? chars c)
                           (string c)
                           (string-append "_" (number->string (char->integer c)) "C"))) xs)
                (list "Z"))))))
(define (with-exception-handler handler thunk)
  (with-handlers ([(λ (x) #t) handler]) (thunk)))
(define genstr!
  (let ([c 0])
    (λ ()
      (set! c (+ c 1))
      (string-append "g" (number->string c)))))
(struct atom ([v #:mutable]))
(define gms (hash))
(define genv
  (hash
   "raise" raise
   "with-exception-handler" with-exception-handler

   "eq?" equal?
   "equal?" equal?
   "eval" (λ (x) (EVAL gms genv x))
   "apply" apply
   "procedure?" procedure?

   "car" car
   "cdr" cdr
   "cons" cons
   "pair?" pair?
   "null?" null?
   "list?" list?
   "list" list
   "length" (λ (x) (exact->inexact (length x)))
   "map" map
   "append" append

   "void" (void)
   "void?" void?

   "string?" string?
   "string-append" string-append
   "str->strlist" (λ (s) (map string (string->list s)))
   "genstr!" genstr!
   "id" id

   "number?" number?
   "+" +
   "-" -
   "*" *
   "/" /
   "number->string" (λ (x) (if (integer? x)
                               (number->string (inexact->exact x))
                               (number->string x)))
   "string->number" string->number
   "<" <
   ">" >
   "=" =
   "<=" <=
   ">=" >=

   "newmap" hash
   "map?" hash?
   "map-set" (λ (m k v) (hash-set
                         (if (immutable? m)
                             m
                             (make-immutable-hash (hash->list m)))
                         k
                         v))
   "map-ref" hash-ref
   "list->map" make-immutable-hash
   "map->list" hash->list
   "newmap!" (λ xs (make-hash (hash->list (apply hash xs))))
   "list->map!" make-hash
   
   "atom!" atom
   "atom-get" atom-v
   "atom-set!" set-atom-v!
   "atom-map!" (λ (f a) (let ([x (f (atom-v a))])
                          (set-atom-v! a x)
                          x))
   "atom-get/set!" (λ (a f) (let ([x (f (atom-v a))])
                              (set-atom-v! a (cdr x))
                              (car x)))
   "atom?" atom?

   "__MK_REC__" (λ (name c)
                  (let-values ([(st make ? ref rset!)
                                (make-struct-type (string->symbol name) #f (inexact->exact c) 0)])
                    (list make
                          ?
                          (λ (x k) (ref x (inexact->exact k)))
                          (λ (x k v) (rset! x (inexact->exact k) v)))))
   "PRELUDE" (delay (QUOTE (sexp-> prelude)))
   ))
(define prelude (include/quote/list "prelude.cscm"))
(run-sexps (append csprelude prelude '(__PRELUDE_END__)))
