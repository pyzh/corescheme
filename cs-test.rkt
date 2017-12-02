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
(require "cs.rkt")
(define-syntax-rule (test [c r] ...)
  (begin
    (check-equal? (run-sexp (quote c)) r) ...))
(test
 [`(list ,(+ 1 2) 4) '(list 3 4)]
 [(let ((name 'a)) `(list ,name ',name)) '(list a (quote a))]
 [`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) '(a 3 4 5 6 b)]
 [`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) '((foo 7) . cons)]
 [(let ((foo '(foo bar)) (@baz 'baz))
    `(list ,@foo , @baz)) '(list foo bar baz)]
 [`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)]
 [(let ((name1 'x)
        (name2 'y))
    `(a `(b ,,name1 ,',name2 d) e)) '(a `(b ,x ,'y d) e)]
 )
(test
 [(add-between '(x y z) 'and) '(x and y and z)]
 [(add-between '(x) 'and) '(x)])
(test
 [(begin
    (define-record-type <pare>
      (kons x y)
      pare?
      (x kar set-kar!)
      (y kdr))
    (list (pare? (kons 1 2))
          (pare? (cons 1 2))
          (kar (kons 1 2))
          (kdr (kons 1 2))
          (let ((k (kons 1 2)))
            (set-kar! k 3)
            (kar k)))) '(#t #f 1 2 3)])
