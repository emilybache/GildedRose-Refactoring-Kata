(include "assert.scm")
(include "gilded-rose.scm")

(test-case "foo"
    (let ((items (list (make-item "foo" 0 0))))
        (update-quality items)
        (assert-string= "fixme" (item-name (car items)))))
