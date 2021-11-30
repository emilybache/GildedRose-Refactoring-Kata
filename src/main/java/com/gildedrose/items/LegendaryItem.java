package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

public class LegendaryItem implements ItemType {

  public static final int LEGENDARY_ITEM_QUALITY = 80;
  public static final String LEGENDARY = "Sulfuras, Hand of Ragnaros";

  private final Item item;

  public LegendaryItem(Item item) {
    this.item = item;
  }


  @Override
  public void updateQuality() {
    decrementSellInDate();
  }

  @Override
  public void validateQuality() {
    if (isLegendaryWrongQuality(item)) {
      throw new IllegalArgumentException(NOT_LEGENDARY_ITEM_ERROR_MESSAGE + item.quality);
    }
  }

  @Override
  public String getName() {
    return LEGENDARY;
  }

  public void decrementSellInDate() {
    item.sellIn--;
  }

  public static boolean isLegendaryWrongQuality(Item item) {
    return item.name.equals(LEGENDARY) && item.quality != LEGENDARY_ITEM_QUALITY;
  }

}
