package com.gildedrose.items;

import com.gildedrose.Quality;
import com.gildedrose.SellIn;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
public class BackstagePassItem extends GildedRoseItem {

  public BackstagePassItem(SellIn sellIn, Quality quality, boolean conjured) {
    super("Backstage passes to a TAFKAL80ETC concert", sellIn, quality, conjured);
  }

  @Override
  protected Quality nextQuality(Quality previous) {
    Quality nextQuality = previous.copy();
    if (this.getSellIn().isOver()) {
      nextQuality.setValue(0);
      return nextQuality;
    }

    nextQuality.increment();
    if (this.getSellIn().daysLeftUntilIsOverAreLessThan(11)) {
      nextQuality.increment();
    }
    if (this.getSellIn().daysLeftUntilIsOverAreLessThan(6)) {
      nextQuality.increment();
    }
    return nextQuality;
  }
}
