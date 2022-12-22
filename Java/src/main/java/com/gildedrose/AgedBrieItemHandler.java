package com.gildedrose;

public class AgedBrieItemHandler extends ItemHandler {

  public AgedBrieItemHandler() {}

  @Override
  public void handleDay(Item item) {
    if (item.quality != 50) {
      item.quality += 1;
    }
    item.sellIn -= 1;
  }
}
