;;;; gilded-rose.asd

(defsystem "gilded-rose"
  :description "Gilded Rose is a small inn selling the finest goods."
  :author "Leeroy <leeroy@gilded-rose.com>"
  :version "1.0.0"
  :pathname "source/"
  :components ((:file "package")
               (:file "gilded-rose" :depends-on ("package")))
  :in-order-to ((test-op (test-op "gilded-rose/tests"))))

(defsystem "gilded-rose/tests"
  :description "Unit tests for gilded-rose-package"
  :author "Leeroy <leeroy@gilded-rose.com>"
  :version "1.0.0"
  :depends-on ("gilded-rose" "parachute" "cl-mock")
  :pathname "tests/"
  :components ((:file "package")
	       (:file "tests" :depends-on ("package")))
  :perform (test-op (o c) (symbol-call :parachute :test :gilded-rose-tests)))
