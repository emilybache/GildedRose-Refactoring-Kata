package com.gildedrose;

import java.util.Arrays;

class GildedRose {

  private Item[] items;

  public GildedRose(Item[] items) {
    this.items = items;
  }

  public void updateQuality() {
    Arrays.stream(items).forEach(this::updateQuality);
  }

  void updateQuality(Item item) {
    Category category = Category.fromName(item.name);

    if (category != Category.SULFURAS) {
      item.sellIn = item.sellIn - 1;
      QualityApplierFactory.applyQuality(item, category);
    }
  }
}
