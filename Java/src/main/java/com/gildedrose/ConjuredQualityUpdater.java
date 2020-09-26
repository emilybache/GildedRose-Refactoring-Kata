package com.gildedrose;

public class ConjuredQualityUpdater extends AbstractItemQualityUpdater {

   private final int dailyQualityDegradation = 2;

   public ConjuredQualityUpdater(Item item) {
      super(item);
   }


   @Override
   public void updateQuality() {
      decreaseQuality(dailyQualityDegradation);

      item.sellIn = item.sellIn - 1;

      if (item.sellIn < 0) {
         decreaseQuality(dailyQualityDegradation);
      }
   }
}
