package com.gildedrose;

import static com.gildedrose.ItemQualityUpdateStrategyFactory.itemQualityUpdaterFor;

import java.util.Arrays;

class GildedRose {

   Item[] items;

   public GildedRose(Item[] items) {
      this.items = items;
   }

   public void updateQuality() {
      Arrays.stream(items).forEach(this::updateQuality);
   }

   private void updateQuality(Item item) {
      itemQualityUpdaterFor(item).updateQuality();
   }

}