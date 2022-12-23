package com.gildedrose;

public class ConjuredItemHandler implements ItemHandler {

  @Override
  public void handleDay(Item item) {
    item.quality -= 2;
    item.sellIn -= 1;
    if (item.sellIn < 0) {
      item.quality -= 2;
    }
  }
}
