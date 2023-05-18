(ns gilded.main
  (:require [gilded.core :as x]))

(defn item->str [item]
  (str (:name item) ", " (:sell-in item) ", " (:quality item)))

(defn print-store [store]
  (println "name, sellIn, quality")
  (doseq [item (x/item-seq store)]
    (println (item->str item)))
  (println))

(def fixture
  [{:name "Sports Memorabilia", :quality 20, :sell-in 10}
   {:name "Aged Cheese", :quality 0, :sell-in 2}
   {:name "Coffee Table Book", :quality 7, :sell-in 5}
   {:name "Fine Italian Silk", :quality 80, :sell-in 0}
   {:name "Fine Italian Silk", :quality 80, :sell-in -1}
   {:name "Backstage passes to a concert", :quality 20, :sell-in 15}
   {:name "Backstage passes to a concert", :quality 49, :sell-in 10}
   {:name "Backstage passes to a concert", :quality 49, :sell-in 5}
   {:name "Baked Chocolate Cake", :quality 6, :sell-in 3}])

(defn -main [& args]
  (let [n-days (if (nil? (first args))
                 2
                 (Long/parseLong (first args)))
        store (x/make-store fixture)]
    (dotimes [day n-days]
      (println "-------- day" day "--------")
      (print-store store)
      (x/update-quality! store))))
