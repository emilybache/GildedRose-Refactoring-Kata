package com.gildedrose.items;

import com.gildedrose.Quality;
import com.gildedrose.SellIn;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
public class AgedBrieItem extends GildedRoseItem {

  public AgedBrieItem(SellIn sellIn, Quality quality, boolean conjured) {
    super("Aged Brie", sellIn, quality, conjured);
  }

  @Override
  protected Quality nextQuality(Quality previous) {
    Quality nextQuality = previous.copy();
    nextQuality.increment();
    return nextQuality;
  }
}
