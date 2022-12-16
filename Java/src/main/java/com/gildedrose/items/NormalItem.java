package com.gildedrose.items;

import com.gildedrose.Quality;
import com.gildedrose.SellIn;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
public class NormalItem extends GildedRoseItem {

  public NormalItem(String name, SellIn sellIn, Quality quality, boolean conjured) {
    super(name, sellIn, quality, conjured);
  }

  @Override
  protected Quality nextQuality(Quality previous) {
    Quality nextQuality = previous.copy();
    nextQuality.decrement();
    return nextQuality;
  }
}
