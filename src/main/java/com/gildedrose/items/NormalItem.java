package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

public class NormalItem implements ItemType {

  public static final String NORMAL = "Normal";
  private final ItemHandler item;

  public NormalItem(Item item) {
    this.item = new ItemHandler(item);
  }

  @Override
  public void updateQuality() {
    item.decrementSellInDate();
    if (item.beforeSellInDate()) {
      item.decrementQuality();
    } else {
      item.decrementQualityBy2();
    }
  }

  @Override
  public String getName() {
    return NORMAL;
  }

}
