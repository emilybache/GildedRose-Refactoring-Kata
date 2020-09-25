package com.gildedrose;

public class AgedBrieQualityUpdater implements ItemQualityUpdater {

   private final Item item;

   public AgedBrieQualityUpdater(Item item) {
      this.item = item;
   }

   @Override
   public void updateQuality() {
      if (item.quality < 50) {
         item.quality = item.quality + 1;
      }

      item.sellIn = item.sellIn - 1;

      if (item.sellIn < 0) {
         if (item.quality < 50) {
            item.quality = item.quality + 1;
         }
      }

   }
}
