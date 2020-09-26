package com.gildedrose;

public class DefaultQualityUpdater extends AbstractItemQualityUpdater {
   private final int dailyQualityDegradation = 1;

   public DefaultQualityUpdater(Item item) {
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
