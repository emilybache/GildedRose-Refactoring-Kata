package com.gildedrose.main;

import com.gildedrose.item_helpers.ItemType;

import static com.gildedrose.item_helpers.ItemFactory.getItemType;
import static java.util.Arrays.stream;

public class GildedRose {
  Item[] items;

  public GildedRose(Item[] items) {
    this.items = items;
  }

  public void updateQuality() {
    stream(items).forEach(item -> {
      ItemType itemType = getItemType(item);
      itemType.validateQuality();
      itemType.updateQuality();
    });
  }
}
