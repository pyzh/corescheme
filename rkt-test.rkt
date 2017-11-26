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
(require rackunit)
(require "rkt.rkt")
(define-syntax-rule (test [c r] ...)
  (begin
    (check-equal? (run-sexp (quote c)) r) ...))
(test
 [`(list ,(+ 1 2) 4) '(list 3.0 4.0)]
 [(let ((name 'a)) `(list ,name ',name)) '(list a (quote a))]
 [`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) '(a 3.0 4.0 5.0 6.0 b)]
 [`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) '((foo 7.0) . cons)]
 [(let ((foo '(foo bar)) (@baz 'baz))
    `(list ,@foo , @baz)) '(list foo bar baz)]
 )
