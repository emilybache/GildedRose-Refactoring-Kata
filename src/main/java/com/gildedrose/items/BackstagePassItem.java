package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

public class BackstagePassItem implements ItemType {

  public static final String BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert";
  private final ItemHandler item;

  public BackstagePassItem(Item item) {
    this.item = new ItemHandler(item);
  }

  @Override
  public void updateQuality() {
    item.decrementSellInDate();
    if (item.moreThan10DaysToSellIn()) {
      item.incrementQuality();
    } else if (item.lessThan10DaysToSellIn()) {
      item.incrementQualityBy2();
    } else if (item.lessThan5DaysToSellIn()) {
      item.incrementQualityBy3();
    } else {
      item.makeQualityZero();
    }
  }

  @Override
  public String getName() {
    return BACKSTAGE_PASS;
  }

}
