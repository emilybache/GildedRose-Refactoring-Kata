;;;; package.lisp

(defpackage :gilded-rose-tests
  (:use :cl :gilded-rose)
  (:import-from :parachute
   :define-test
   :is)
  (:import-from :cl-mock
   :with-mocks
   :answer
   :call-previous
   :invocations))
