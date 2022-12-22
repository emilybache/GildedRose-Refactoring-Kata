package com.gildedrose;

public class AgedBrieItem {
  public static void handleDay(Item item) {
    if (item.quality != 50) {
      item.quality += 1;
    }
    item.sellIn -= 1;
  }
}
