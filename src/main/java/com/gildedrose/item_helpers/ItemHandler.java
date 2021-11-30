package com.gildedrose.item_helpers;

import com.gildedrose.main.Item;

import static java.lang.Math.max;

public class ItemHandler {

  private final Item item;

  public ItemHandler(Item item) {
    this.item = item;
  }

  public void decrementSellInDate() {
    item.sellIn--;
  }

  public boolean beforeSellInDate() {
    return item.sellIn >= 0;
  }

  public void decrementQuality() {
    item.quality = max(item.quality - 1, 0);
  }

  public void decrementQualityBy2() {
    item.quality = max(item.quality - 2, 0);
  }

  public void decrementQualityBy4() {
    item.quality = max(item.quality - 4, 0);
  }

}
