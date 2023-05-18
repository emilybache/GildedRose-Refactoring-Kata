(ns gilded.core)

(defn make-store [items]
  (assert (vector? items))
  (->> items
       (map (fn [item] (atom item)))
       vec))

(defn item-seq [store]
  (->> store
       (map deref)))
;; ---

(defn update-quality! [store]
  (doseq [item store]
    (if (and (not (= (:name @item)
                     "Aged Cheese"))
             (not (= (:name @item)
                     "Backstage passes to a concert")))

      (when (> (:quality @item) 0)
        (when (not (= (:name @item) "Fine Italian Silk"))
          (swap! item update :quality #(- % 1))))

      (when (< (:quality @item) 50)
        (swap! item update :quality #(+ % 1))
        (when (= (:name @item) "Backstage passes to a concert")
          (when (< (:sell-in @item) 11)
            (when (< (:quality @item) 50)
              (swap! item update :quality #(+ % 1))))
          (when (< (:sell-in @item) 6)
            (when (< (:quality @item) 50)
              (swap! item update :quality #(+ % 1)))))))

    (when (not (= (:name @item) "Fine Italian Silk"))
      (swap! item update :sell-in #(- % 1)))

    (when (< (:sell-in @item) 0)
      (if (not (= (:name @item) "Aged Cheese"))
        (if (not (= (:name @item) "Backstage passes to a concert"))
          (when (> (:quality @item) 0)
            (when (not (= (:name @item) "Fine Italian Silk"))
              (swap! item update :quality #(- % 1))))
          (swap! item update :quality #(- % %)))
        (when (< (:quality @item) 50)
          (swap! item update :quality #(+ % 1)))))))
