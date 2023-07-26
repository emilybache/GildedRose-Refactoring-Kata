(in-package :gilded-rose-tests)

(define-test gilded-rose-testsuite)

(define-test "An item in gilded-rose is updated."
  :parent gilded-rose-testsuite
  (let* ((an-item (make-instance 'item :name "An item" :sell-in 1 :quality 1))
	 (some-items (list an-item))
	 (my-app (make-instance 'gilded-rose :items some-items)))
    (update-quality my-app)
    (is equal (slot-value (first (slot-value my-app 'items)) 'name) "Fix me")))

