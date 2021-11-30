package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

import static java.lang.Math.min;

public class AgedBrieItem implements ItemType {

  public static final String AGED_BRIE = "Aged Brie";

  private final Item item;

  public AgedBrieItem(Item item) {
    this.item = item;
  }

  @Override
  public void updateQuality() {
    decrementSellInDate();
    if (beforeSellInDate()) {
      incrementQuality();
    } else {
      incrementQualityBy2();
    }
  }

  public void incrementQuality() {
    item.quality = min(item.quality + 1, 50);
  }


  public void incrementQualityBy2() {
    item.quality = min(item.quality + 2, 50);
  }

  public void decrementSellInDate() {
    item.sellIn--;
  }

  public boolean beforeSellInDate() {
    return item.sellIn >= 0;
  }

  @Override
  public String getName() {
    return AGED_BRIE;
  }

}
