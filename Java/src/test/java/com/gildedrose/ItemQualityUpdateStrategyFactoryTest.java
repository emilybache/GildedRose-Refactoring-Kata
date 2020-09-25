package com.gildedrose;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class ItemQualityUpdateStrategyFactoryTest {

   @Test
   void itemQualityUpdaterFor() {
      testFactoryFor("Sulfuras, Hand of Ragnaros", SulfrasQualityUpdater.class);
      testFactoryFor("Aged Brie", AgedBrieQualityUpdater.class);
      testFactoryFor("Backstage passes to a TAFKAL80ETC concert", BackstageBrieQualityUpdater.class);
      testFactoryFor("xyz", DefaultQualityUpdater.class);
   }

   void testFactoryFor(String name, Class<? extends ItemQualityUpdater> clazz) {
      ItemQualityUpdater qualityUpdater = ItemQualityUpdateStrategyFactory
          .itemQualityUpdaterFor(new Item(name, 0, 0));
      assertThat(qualityUpdater).isInstanceOf(clazz);
   }

}