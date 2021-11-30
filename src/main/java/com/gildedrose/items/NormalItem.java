package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

import static java.lang.Math.max;

public class NormalItem implements ItemType {

  public static final String NORMAL = "Normal";
  private final Item item;

  public NormalItem(Item item) {
    this.item = item;
  }

  @Override
  public void updateQuality() {
    decrementSellInDate();
    if (beforeSellInDate()) {
      decrementQuality();
    } else {
      decrementQualityBy2();
    }
  }

  public void decrementSellInDate() {
    item.sellIn--;
  }

  public boolean beforeSellInDate() {
    return item.sellIn >= 0;
  }

  public void decrementQuality() {
    item.quality = max(item.quality - 1, 0);
  }

  public void decrementQualityBy2() {
    item.quality = max(item.quality - 2, 0);
  }

  @Override
  public String getName() {
    return NORMAL;
  }

}
