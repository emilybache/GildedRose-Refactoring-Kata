(require 'ert)
(require 'gilded-rose)

(setq item-list
      (list (make-item "foo" 5 5)))

(ert-deftest check-name-of-item ()
  (should (string= "fixme" (plist-get (car item-list) :name))))

(ert-run-tests-interactively t)
