(defmodule gilded-rose-test
  (behaviour ltest-unit)
  (export all)
  (import (from gilded-rose
                (update-quality 1)
                (make-item 3)
                (item-name 1))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest update-quality-test
  (is-equal "foo" (item-name (car (update-quality
                                   (list (make-item "foo" 0 0)))))))
