package com.gildedrose.items;

import com.gildedrose.Quality;
import com.gildedrose.SellIn;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
public class SulfurasItem extends GildedRoseItem {

  static final Quality DEFAULT_QUALITY = Quality.create(80);

  public SulfurasItem(SellIn sellIn) {
    super("Sulfuras", sellIn, DEFAULT_QUALITY, Boolean.FALSE);
  }

  @Override
  protected Quality nextQuality(Quality previous) {
    // being a legendary item, never has to be sold or decreases in Quality
    return previous.copy();
  }
}
