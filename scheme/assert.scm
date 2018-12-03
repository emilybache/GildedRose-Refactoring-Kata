;;;
;;; Unit test framework for Scheme
;;; Copyright (c) 2018, Peter Kofler, http://www.code-cop.org/
;;; BSD licensed.
;;;
;;; Non S5RS used functions:
;;; * (error) from R6RS
;;;

;; SchemeUnit from http://c2.com/cgi/wiki?SchemeUnit

(define (fail msg)
    (error (string-append "AssertionError" ": " msg)))

(define (check msg condition)
    (if (not condition)
        (fail msg)
        #t))

(define (assert msg condition)
    (lambda () (check msg condition)))

;; extensions

;; private
(define (make-string-message prefix to-string expected actual)
    (make-message prefix
                  (to-string expected)
                  (to-string actual)))

;; private
(define (make-message prefix expected actual)
    (string-append prefix "expected:<" expected "> but was:<" actual ">"))

(define (assert-equal to-string eq-op expected actual)
    (assert (make-string-message "" to-string expected actual)
            (eq-op expected actual)))

(define (assert= expected actual)
    (assert-equal number->string = expected actual))

(define (assert-string= expected actual)
    (assert-equal values string=? expected actual))

;; private
(define (test-case-name name)
    (display name)
    (display " ... "))

;; private
(define (test-case-success)
    (display "OK")
    (newline))

(define (test-case name . assertions)
    (test-case-name name)
    (for-each (lambda (a) (a)) assertions)
    (test-case-success))
