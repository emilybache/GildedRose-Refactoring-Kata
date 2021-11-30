package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemHandler;
import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

public class LegendaryItem implements ItemType {

  public static final int LEGENDARY_ITEM_QUALITY = 80;
  public static final String LEGENDARY = "Sulfuras, Hand of Ragnaros";

  private final ItemHandler item;

  public LegendaryItem(Item item) {
    this.item = new ItemHandler(item);
  }

  @Override
  public void updateQuality() {
    item.decrementSellInDate();
  }

  @Override
  public String getName() {
    return LEGENDARY;
  }

  public static boolean isLegendary(Item item) {
    return item.name.equals(LEGENDARY);
  }

  public static boolean isNotLegendary(Item item) {
    return !item.name.equals(LEGENDARY);
  }

}
