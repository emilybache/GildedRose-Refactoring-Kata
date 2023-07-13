(import ../src/main :as shop)

(defn get-items []
  [ (shop/item "+5 Dexterity Vest" 20 10) 
    (shop/item "Aged Brie" 0 2)
    (shop/item "Elixir of the Mongoose" 7 5)
    (shop/item "Sulfuras, Hand of Ragnaros" 80 0)
    (shop/item "Sulfuras, Hand of Ragnaros" 80 -1)
    (shop/item "Backstage passes to a TAFKAL80ETC concert" 20 15)
    (shop/item "Backstage passes to a TAFKAL80ETC concert" 49 10)
    (shop/item "Backstage passes to a TAFKAL80ETC concert" 49 5)
    (shop/item "Conjured Mana Cake" 6 3)])

# judge allows for snapshot testing of stdout, much like the texttest tool. 
# So something like the following would create a janet-native test of the output of shop/run:
#
# (test-stdout (shop/run 30 (get-items)))

(defn main [& args]
    (def num-days (scan-number (or (get args 1) "-1")))
    (def items (get-items)) 
    (shop/run num-days items))
