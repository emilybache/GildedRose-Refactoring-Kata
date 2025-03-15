(ns gilded.core-test
  (:require [clojure.test :refer [deftest is]]
            [gilded.core :as x]))

(def fixture
  [{:name "foo", :quality 20, :sell-in 10}])

(deftest simple-test
  (let [store (x/make-store fixture)
        _ (x/update-quality! store)
        item (first (x/item-seq store))]
    (is (= "fixme" (:name item)))))
