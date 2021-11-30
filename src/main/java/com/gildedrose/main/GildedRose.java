package com.gildedrose.main;

import static com.gildedrose.item_helpers.ItemFactory.getItemType;
import static java.util.Arrays.stream;

public class GildedRose {
  Item[] items;

  public GildedRose(Item[] items) {
    this.items = items;
  }

  public void updateQuality() {
    stream(items).forEach(item -> getItemType(item).updateQuality());
  }
}
