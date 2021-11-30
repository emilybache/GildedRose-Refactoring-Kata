package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

public class AgedBrieItem implements ItemType {

  public static final String AGED_BRIE = "Aged Brie";

  private final ItemHandler item;

  public AgedBrieItem(Item item) {
    this.item = new ItemHandler(item);
  }

  @Override
  public void updateQuality() {
    item.decrementSellInDate();
    if (item.beforeSellInDate()) {
      item.incrementQuality();
    } else {
      item.incrementQualityBy2();
    }
  }

  @Override
  public String getName() {
    return AGED_BRIE;
  }

}
