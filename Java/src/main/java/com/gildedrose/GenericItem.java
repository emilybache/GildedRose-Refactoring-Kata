package com.gildedrose;

public class GenericItem {
  public static void handleDay(Item item) {
    item.sellIn -= 1;
    if (item.quality == 0) {
      return;
    }
    if (item.sellIn < 0) {
      item.quality -= 1;
    }
    item.quality -= 1;
  }
}
