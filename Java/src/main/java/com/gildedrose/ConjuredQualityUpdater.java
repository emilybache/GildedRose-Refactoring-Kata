package com.gildedrose;

public class ConjuredQualityUpdater implements ItemQualityUpdater {

   private final Item item;
   private final int normalDailyQualityDegradation = 2;

   public ConjuredQualityUpdater(Item item) {
      this.item = item;
   }


   @Override
   public void updateQuality() {
      decreaseQuality(normalDailyQualityDegradation);

      item.sellIn = item.sellIn - 1;

      if (item.sellIn < 0) {
         decreaseQuality(normalDailyQualityDegradation);
      }
   }

   void decreaseQuality(int decrementValue) {
      item.quality = item.quality - decrementValue;
      if (item.quality < 0) {
         item.quality = 0;
      }
   }
}
