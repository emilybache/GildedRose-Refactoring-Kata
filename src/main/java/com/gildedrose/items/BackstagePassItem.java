package com.gildedrose.items;

import com.gildedrose.item_helpers.ItemType;
import com.gildedrose.main.Item;

import static com.gildedrose.item_helpers.ItemType.qualityIsAbove50;
import static com.gildedrose.item_helpers.ItemType.qualityIsNegative;
import static java.lang.Math.min;

public class BackstagePassItem implements ItemType {

  public static final String BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert";

  private final Item item;

  public BackstagePassItem(Item item) {
    this.item = item;
  }

  @Override
  public void updateQuality() {
    decrementSellInDate();
    if (moreThan10DaysToSellIn()) {
      incrementQuality();
    } else if (lessThan10DaysToSellIn()) {
      incrementQualityBy2();
    } else if (lessThan5DaysToSellIn()) {
      incrementQualityBy3();
    } else {
      makeQualityZero();
    }
  }

  @Override
  public void validateQuality() {
    if (qualityIsNegative(item)) {
      throw new IllegalArgumentException(QUALITY_ERROR_MESSAGE + item.quality);
    } else if (qualityIsAbove50(item))
      throw new IllegalArgumentException(OUT_OF_BOUND_QUALITY_MESSAGE + item.quality);
  }

  public void decrementSellInDate() {
    item.sellIn--;
  }

  public boolean lessThan5DaysToSellIn() {
    return item.sellIn >= 0 && item.sellIn <= 5;
  }

  public boolean lessThan10DaysToSellIn() {
    return item.sellIn >= 5 && item.sellIn <= 10;
  }

  public boolean moreThan10DaysToSellIn() {
    return item.sellIn >= 10;
  }

  public void makeQualityZero() {
    item.quality = 0;
  }

  public void incrementQuality() {
    item.quality = min(item.quality + 1, 50);
  }

  public void incrementQualityBy2() {
    item.quality = min(item.quality + 2, 50);
  }

  public void incrementQualityBy3() {
    item.quality = min(item.quality + 3, 50);
  }

  @Override
  public String getName() {
    return BACKSTAGE_PASS;
  }

}
