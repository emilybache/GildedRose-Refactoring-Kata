package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

import static com.gildedrose.item_helpers.ItemType.qualityIsAbove50;
import static com.gildedrose.item_helpers.ItemType.qualityIsNegative;
import static java.lang.Math.max;

public class ConjuredItem implements ItemType {

  public static final String CONJURED = "Conjured Mana Cake";

  private final Item item;

  public ConjuredItem(Item item) {
    this.item = item;
  }

  @Override
  public void updateQuality() {
    decrementSellInDate();
    if (beforeSellInDate()) {
      decrementQualityBy2();
    } else {
      decrementQualityBy4();
    }
  }

  @Override
  public void validateQuality() {
    if (qualityIsNegative(item)) {
      throw new IllegalArgumentException(QUALITY_ERROR_MESSAGE + item.quality);
    } else if (qualityIsAbove50(item)) {
      throw new IllegalArgumentException(OUT_OF_BOUND_QUALITY_MESSAGE + item.quality);
    }
  }

  public void decrementSellInDate() {
    item.sellIn--;
  }

  public boolean beforeSellInDate() {
    return item.sellIn >= 0;
  }

  public void decrementQualityBy2() {
    item.quality = max(item.quality - 2, 0);
  }

  public void decrementQualityBy4() {
    item.quality = max(item.quality - 4, 0);
  }

  @Override
  public String getName() {
    return CONJURED;
  }

}
