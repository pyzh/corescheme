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
(provide run-xs run run-sexps run-sexp sexp-> ->sexp)
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

(define (run-xs xs) (unQUOTE (EVAL (hash) genv (QUOTE (append prelude xs)))))
(define (run x) (run-xs (list x)))
(define (run-sexps xs) (->sexp (run-xs (map sexp-> xs))))
(define (run-sexp x) (run-sexps (list x)))
(define QUOTE
  (match-lambda
    [(and x (or (? boolean?) (? string?) (? number?) '())) x]
    [(list a ... "." d) `(,@(map QUOTE a) . ,(QUOTE d))]
    [(list x ...) (map QUOTE x)]))
(define unQUOTE
  (match-lambda
    [(and x (or (? boolean?) (? string?) (? number?) '())) x]
    [(list-rest a ... d) `(,@(map unQUOTE a) "." ,(unQUOTE d))]
    [(list x ...) (map unQUOTE x)]))
(define sexp->
  (match-lambda
    [(? symbol? x) (symbol->string x)]
    [(? string? x) (list "quote" x)]
    [(? number? x) (+ x 0.0)]
    [(? boolean? x) x]
    ['() '()]
    [(list x ...) (map sexp-> x)]
    [(list-rest a ... d) `(,@(map sexp-> a) "." ,(sexp-> d))]))
(define ->sexp
  (match-lambda
    [(? string? x) (string->symbol x)]
    [(and x (or (? number?) (? boolean?) '())) x]
    [(list a ... "." d) `(,@(map ->sexp a) . ,(->sexp d))]
    [(list x ...) (map ->sexp x)]))
(define (macroexpand ms x)
  (let ([b (and (pair? x) (hash-ref ms (car x) #f))])
    (if b
        (macroexpand ms (apply (force b) (cdr x)))
        x)))
(define (EVAL ms env x)
  (match (macroexpand ms x)
    [`("if" ,b ,x ,y)
     (if (EVAL ms env b)
         (EVAL ms env x)
         (EVAL ms env y))]
    [`("quote" ,x) x]
    [`(,(or "lambda" "λ") ,args ,@b) (λ xs (BEGIN ms (unify env args xs) b))]
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
          [`(begin ,@b) (preBEGIN ms (append b xs))]
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
    (let-values ([(defs xs) (partition (match-lambda [`("define" ,s ,@v) #t] [_ #f]) xs)])
      (let-values
          ([(defs) (map DEFINE defs)]
           [(macs xs) (partition (match-lambda [`("defmacro" ,s ,@v) #t] [_ #f]) xs)])
        (let ([macs (map defmacro macs)])
          (letrec
              ([ndefs
                (map (λ (x)
                       (cons (car x) (delay (EVAL newms newenv (cdr x))))) defs)]
               [newenv (hash-append env ndefs)]
               [nmacs
                (map (λ (x)
                       (cons (car x) (delay (EVAL newms newenv (cdr x))))) macs)]
               [newms (hash-append ms nmacs)])
            (if (null? nmacs)
                (last (map (λ (x) (EVAL newms newenv x)) xs))
                (BEGIN newms newenv xs))))))))
(define (COND ms env xs)
  (match (car xs)
    [`["else" ,@x] (match (cdr xs) ['() (BEGIN ms env x)])]
    [`[,c ,@x] (if (EVAL ms env c)
                   (BEGIN ms env x)
                   (COND ms env (cdr xs)))]))
(define prelude (cons "begin" (sexp-> (include/quote/list "prelude.cscm"))))
(struct atom ([v #:mutable]))
(define genv
  (hash
   "eq?" equal?
   "equal?" equal?
   "eval" (λ (x) (EVAL (make-hash) genv x))
   "apply" apply
   "procedure?" procedure?

   "car" car
   "cdr" cdr
   "cons" cons
   "pair?" pair?
   "null?" null?
   "list?" list?
   "list" list
   "map" map
   "append" append

   "void" (void)
   "void?" void?

   "string?" string?
   "string-append" string-append
   "str->strlist" (λ (s) (map string (string->list s)))
   "genstr" (λ () (symbol->string (gensym)))
   "->azAZ09_" (λ (s)
                (apply string-append
                       (map
                        (λ (x) (if (or (char-alphabetic? x) (char-numeric? x))
                                   (string x)
                                   (string-append "_" (number->string (char->integer x)))))
                        (string->list s))))

   "number?" number?
   "+" +
   "-" -
   "*" *
   "/" /
   "number->string" number->string
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
   "atom?" atom?
   ))
