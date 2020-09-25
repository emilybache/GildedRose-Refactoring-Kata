package com.gildedrose;

public class ItemQualityUpdateStrategyFactory {
   public static ItemQualityUpdater itemQualityUpdaterFor(Item item) {
      switch (item.name) {
         case "Sulfuras, Hand of Ragnaros": return new SulfrasQualityUpdater(item);
         case "Aged Brie": return new AgedBrieQualityUpdater(item);
         case "Backstage passes to a TAFKAL80ETC concert": return new BackstageBrieQualityUpdater(item);
         default: return new DefaultQualityUpdater(item);
      }
   }
}
