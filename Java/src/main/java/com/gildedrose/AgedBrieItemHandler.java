package com.gildedrose;

public class AgedBrieItemHandler implements ItemHandler {

  public AgedBrieItemHandler() {}

  @Override
  public void handleDay(Item item) {
    if (item.quality != 50) {
      item.quality += 1;
    }
    item.sellIn -= 1;
  }
}
