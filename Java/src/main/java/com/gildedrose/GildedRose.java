package com.gildedrose;

import java.util.Arrays;
import java.util.List;

class GildedRose {
  public static final String AGED_BRIE = "Aged Brie";
  public static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
  public static final String BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";

  final List<Item> items;

  public GildedRose(Item[] items) {
    this.items = Arrays.asList(items);
  }

  public void updateQuality() {
    items.forEach(GildedRose::handleDay);
  }

  private static void handleDay(Item item) {
    switch (item.name) {
      case AGED_BRIE:
        AgedBrieItem.handleDay(item);
        return;
      case SULFURAS:
        return;
      case BACKSTAGE_PASSES:
        BackstagePassesItem.handleDay(item);
        return;
      default:
        GenericItem.handleDay(item);
    }
  }
}
