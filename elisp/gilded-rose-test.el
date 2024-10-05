(require 'ert)
(require 'gilded-rose)

(ert-deftest test-aged-brie-increases-quality ()
  "Test that Aged Brie increases in quality."
  (let ((item (make-item "Aged Brie" 2 0)))
    (update-quality item)
    (should (= (item-quality item) 1))))

(ert-deftest test-aged-brie-max-quality ()
  "Test that Aged Brie does not exceed max quality."
  (let ((item (make-item "Aged Brie" 2 50)))
    (update-quality item)
    (should (= (item-quality item) 50))))

(ert-deftest test-backstage-passes-increases-quality ()
  "Test that Backstage passes increase in quality."
  (let ((item (make-item "Backstage passes" 15 20)))
    (update-quality item)
    (should (= (item-quality item) 20))))

(ert-deftest test-backstage-passes-quality-increases-by-2 ()
  "Test that Backstage passes increase by 2 when sell-in is 10 or less."
  (let ((item (make-item "Backstage passes" 10 20)))
    (update-quality item)
    (should (= (item-quality item) 22))))

(ert-deftest test-backstage-passes-quality-increases-by-3 ()
  "Test that Backstage passes increase by 3 when sell-in is 5 or less."
  (let ((item (make-item "Backstage passes" 5 20)))
    (update-quality item)
    (should (= (item-quality item) 23))))

(ert-deftest test-backstage-passes-quality-drops-to-0 ()
  "Test that Backstage passes drop to 0 after sell-in date."
  (let ((item (make-item "Backstage passes" 0 20)))
    (update-quality item)
    (should (= (item-quality item) 0))))

(ert-deftest test-normal-item-decreases-quality ()
  "Test that normal items decrease in quality."
  (let ((item (make-item "Normal Item" 5 10)))
    (update-quality item)
    (should (= (item-quality item) 9))))

(ert-deftest test-normal-item-quality-decreases-twice-after-sell-in ()
  "Test that normal items decrease in quality by 2 after sell-in date."
  (let ((item (make-item "Normal Item" 0 10)))
    (update-quality item)
    (should (= (item-quality item) 8))))

(ert-deftest test-sulfuras-quality-unchanged ()
  "Test that Sulfuras quality remains unchanged."
  (let ((item (make-item "Sulfuras" 0 80)))
    (update-quality item)
    (should (= (item-quality item) 80))))

(ert-deftest test-normal-item-quality-never-negative ()
  "Test that normal items' quality never goes negative."
  (let ((item (make-item "Normal Item" 0 0)))
    (update-quality item)
    (should (= (item-quality item) 0))))

(ert-deftest simple-test ()
  (should (= 1 1)))

;; Run the tests
(ert-run-tests-interactively t)
