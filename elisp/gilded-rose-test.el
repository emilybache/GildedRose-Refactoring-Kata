(require 'ert)
(require 'gilded-rose)


(defconst foo (make-item "foo" 20 10))

(ert-deftest check-name-of-item ()
    (should (string= "fixme" (plist-get foo :name))))

(ert-run-tests-interactively t)
