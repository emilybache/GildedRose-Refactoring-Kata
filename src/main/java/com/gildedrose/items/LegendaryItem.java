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
  public String getName() {
    return LEGENDARY;
  }

  public void decrementSellInDate() {
    item.sellIn--;
  }

  public static boolean isLegendary(Item item) {
    return item.name.equals(LEGENDARY);
  }

  public static boolean isNotLegendary(Item item) {
    return !item.name.equals(LEGENDARY);
  }

}
