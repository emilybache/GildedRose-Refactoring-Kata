(defmodule gilded-rose-test
  (behaviour ltest-unit)
  (export all)
  (import (from gilded-rose
                (update-quality 1))))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "include/gilded-rose-item.lfe")

(deftest update-quality-test
  (is-equal "fixme" (item-name (car (update-quality
                                     (list (make-item name "foo" sellin 0 quality 0)))))))
