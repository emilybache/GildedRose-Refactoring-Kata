package com.gildedrose;

import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;

class QualityApplierFactory {
  private static final List<QualityApplier> APPLIERS =
      Arrays.asList(
          new DegradingQualityApplier(1, 2, EnumSet.of(Category.OTHER)),
          new DegradingQualityApplier(2, 2, EnumSet.of(Category.CONJURED)),
          new IncreasingQualityApplier(1, 50, EnumSet.of(Category.AGED_BRIE)),
          new BackstagePassQualityApplier(50, EnumSet.of(Category.BACKSTAGE_PASS)));

  private QualityApplierFactory() {
  }

  static void applyQuality(Item item, Category category) {
    APPLIERS.stream()
        .filter(applier -> applier.matches(item, category))
        .forEach(applier -> item.quality = applier.apply(item));
  }
}
