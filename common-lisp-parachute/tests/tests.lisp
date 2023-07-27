(in-package :gilded-rose-tests)

(define-test gilded-rose-testsuite)

(define-test "Test foo."
  :parent gilded-rose-testsuite
  (let* ((an-item (make-instance 'item :name "foo" :sell-in 0 :quality 0))
	 (some-items (list an-item))
	 (my-app (make-instance 'gilded-rose :items some-items)))
    (update-quality my-app)
    (is equal (slot-value (first (slot-value my-app 'items)) 'name) "fixme")))

