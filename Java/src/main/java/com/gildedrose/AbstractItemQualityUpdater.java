package com.gildedrose;

public abstract class AbstractItemQualityUpdater implements ItemQualityUpdater {

   protected Item item;

   public AbstractItemQualityUpdater(Item item) {
      this.item = item;
   }

   /**
    * Decrease quality of an item, but never go below 0
    * @param decrementValue Value the quality should be decreased by
    */
   void decreaseQuality(int decrementValue) {
      item.quality = item.quality - decrementValue;
      if (item.quality < 0) {
         item.quality = 0;
      }
   }
}
