package com.gildedrose;

public class BackstagePassesItem {
  public static void handleDay(Item item) {
      // todo: make it so these items can't be initialized with over 50
      if (item.quality == 50) {
        return;
      }
      if (item.sellIn == 0) {
        item.quality = 0;
      } else if (item.sellIn <= 5) {
        item.quality += 3;
      } else if (item.sellIn <= 10) {
        item.quality += 2;
      }
      item.sellIn -= 1;
    }
}
