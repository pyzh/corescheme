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
(define (run x) (EVAL (make-hash) genv (QUOTE (append prelude (list x)))))
(define (run-sexp x) (run (sexp-> x)))
(define QUOTE
  (match-lambda
    ['() '()]
    [(list a ... "." d) `(,@(map QUOTE a) . ,(QUOTE d))]
    [(list x ...) (map QUOTE x)]
    [(and x (or (? boolean?) (? string?) (? number?))) x]))
(define sexp->
  (match-lambda
    [(? symbol? x) (symbol->string x)]
    [(? string? x) (list "quote" x)]
    [(? number? x) (+ x 0.0)]
    [(? boolean? x) x]
    ['() '()]
    [(list x ...) (map sexp-> x)]
    [(list-rest a ... d) `(,@(map sexp-> a) "." ,(sexp-> d))]))
(define (macroexpand ms x)
  (if (pair? x)
      (let ([a (car x)])
        (cond
          [(equal? a "defmacro")
           (match x
             [`("defmacro" ,f ,@x)
              (let-values ([(f x) (match `(,f ,@x)
                                    [`(,(? string? f) ,x) (values f x)]
                                    [`(,(cons (? string? f) args) ,@x) (values f `("λ" ,args ,@x))])])
                (hash-set! ms f (EVAL ms genv x))
                "void")])]
          [(hash-ref ms a #f) => (λ (m) (macroexpand ms (apply m (cdr x))))]
          [else x]))
      x))
(define (EVAL ms env x)
  (match (macroexpand ms x)
    [`("if" ,b ,x ,y) (if (EVAL ms env b) (EVAL ms env x) (EVAL ms env y))]
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
(define (prebegin ms xs)
  (if (null? xs)
      '()
      (match (macroexpand ms (car xs))
        [`("begin" ,@x) (prebegin ms (append x (cdr xs)))]
        [x (cons x (prebegin ms (cdr xs)))])))
(define (BEGIN ms env xs)
  (let ([xs (prebegin ms xs)])
    (let-values ([(defs rs) (partition (match-lambda [`("define" ,s ,@v) #t] [_ #f]) xs)])
      (let ([defs (map DEFINE defs)])
        (letrec ([ndefs (map (λ (x) (cons (car x) (delay (EVAL ms newenv (cdr x))))) defs)]
                 [newenv (hash-append env ndefs)])
          (last (map (λ (x) (EVAL ms newenv x)) rs)))))))
(define (hash-append hash ps)
  (foldl (λ (p h) (hash-set h (car p) (cdr p))) hash ps))
(define DEFINE
  (match-lambda
    [`("define" ,(and (? string? f)) ,x) (cons f x)]
    [`("define" ,(cons f args) ,@x) (DEFINE `("define" ,f ("λ" ,args ,@x)))]))
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

   "void" (void)
   "void?" void?

   "string?" string?
   "string-append" string-append
   "str->strlist" (λ (s) (map string (string->list s)))
   "genstr" (λ () (symbol->string (gensym)))

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
(define prelude (cons "begin" (sexp-> (include/quote/list "prelude.cscm"))))
(define (COND ms env xs)
  (match (car xs)
    [`["else" ,@x] (match (cdr xs) ['() (BEGIN ms env x)])]
    [`[,c ,@x] (if (EVAL ms env c)
                   (BEGIN ms env x)
                   (COND ms env (cdr xs)))]))
