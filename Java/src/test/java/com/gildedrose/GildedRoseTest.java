package com.gildedrose;

import org.approvaltests.combinations.CombinationApprovals;
import org.junit.jupiter.api.Test;

class GildedRoseTest {

   @Test
   void updateQuality() {
      CombinationApprovals
          .verifyAllCombinations(this::doUpdateQuality,
              new String[]{"foo", "Aged Brie", "Backstage passes to a TAFKAL80ETC concert",
                  "Sulfuras, Hand of Ragnaros"}, //name
              new Integer[]{-1, 0, 5, 6, 11}, //sellIn
              new Integer[]{0, 1, 49, 50} //quantity
          );
   }

   private String doUpdateQuality(String name, int sellIn, int quality) {
      Item[] items = new Item[]{new Item(name, sellIn, quality)};
      GildedRose app = new GildedRose(items);
      app.updateQuality();
      return app.items[0].toString();
   }

}
