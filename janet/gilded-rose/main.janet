(defn update-quality [items]
    (for i 0 (length items)
        (if (and
                (not (= "Aged Brie" ((get items i) :name)))
                (not (= "Backstage passes to a TAFKAL80ETC concert" ((get items i) :name))))
            (if (< 0 ((get items i) :quality))
                (if (not (= "Sulfuras, Hand of Ragnaros" ((get items i) :name)))
                    (put (get items i) :quality (- ((get items i) :quality) 1))))
            (if (> 50 ((get items i) :quality))
                (do 
                    (put (get items i) :quality (+ ((get items i) :quality) 1))
                    (if (= "Backstage passes to a TAFKAL80ETC concert" ((get items i) :name))
                        (do 
                            (if (> 11 ((get items i) :sell-in))
                                (if (> 50 ((get items i) :quality))
                                    (put (get items i) :quality (+ ((get items i) :quality) 1))))
                            (if (> 6 ((get items i) :sell-in))
                                (if (> 50 ((get items i) :quality))
                                    (put (get items i) :quality (+ ((get items i) :quality) 1)))))))))
        (if (not (= "Sulfuras, Hand of Ragnaros" ((get items i) :name)))
            (put (get items i) :sell-in (- ((get items i) :sell-in) 1)))    
        (if (> 0 ((get items i) :sell-in))
            (if (not (= "Aged Brie" ((get items i) :name)))
                (if (not (= "Backstage passes to a TAFKAL80ETC concert" ((get items i) :name)))
                    (if (< 0 ((get items i) :quality))
                        (if (not (= "Sulfuras, Hand of Ragnaros" ((get items i) :name)))
                            (put (get items i) :quality (- ((get items i) :quality) 1))))
                    (put (get items i) :quality (- ((get items i) :quality) ((get items i) :quality))))
                (if (> 50 ((get items i) :quality))
                    (put (get items i) :quality (+ ((get items i) :quality) 1)))))))

(defn item-to-str [x] 
    (string (get x :name) ", " (get x :sell-in) ", " (get x :quality)))

(defn main [& args]
    (def num-days (scan-number (or (get args 1) "2")))
    (print "OMGHAI!")
    (def items 
        [@{ :name "+5 Dexterity Vest" :quality 20 :sell-in 10} 
         @{ :name "Aged Brie" :quality 0 :sell-in 2}
         @{ :name "Elixir of the Mongoose" :quality 7 :sell-in 5}
         @{ :name "Sulfuras, Hand of Ragnaros" :quality 80 :sell-in 0}
         @{ :name "Sulfuras, Hand of Ragnaros" :quality 80 :sell-in -1}
         @{ :name "Backstage passes to a TAFKAL80ETC concert" :quality 20 :sell-in 15}
         @{ :name "Backstage passes to a TAFKAL80ETC concert" :quality 49 :sell-in 10}
         @{ :name "Backstage passes to a TAFKAL80ETC concert" :quality 49 :sell-in 5}
         @{ :name "Conjured Mana Cake" :quality 6 :sell-in 3}])
    (for i 0 num-days 
        (do
            (print (string "-------- day " i " --------"))
            (print "name, sellIn, quality")
            (for j 0 (length items)
                (print (item-to-str (get items j))))
            (print "")
            (update-quality items))))
