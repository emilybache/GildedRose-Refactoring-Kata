(require 'ert)
(require 'gilded-rose)

(ert-deftest check-name-of-item ()
  "Test that item is called sweets. This test should fail!"
  (let ((sweets (make-item "sweets" 5 5)))
    (should (string= (item-name sweets) "You should change this"))))

(ert-run-tests-interactively t)
