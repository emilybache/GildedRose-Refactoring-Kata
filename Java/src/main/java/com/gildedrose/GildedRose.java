package com.gildedrose;

import java.util.Arrays;
import java.util.List;

class GildedRose {
  final List<Item> items;

  public GildedRose(Item[] items) {
    this.items = Arrays.asList(items);
  }

  public void updateQuality() {
    items.forEach(this::handleDay);
  }

  private void handleDay(Item item) {
    ItemHandlerFactory.getItemHandler(item).handleDay(item);
  }
}
