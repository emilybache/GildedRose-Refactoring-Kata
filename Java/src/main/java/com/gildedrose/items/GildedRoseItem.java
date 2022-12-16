package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.Quality;
import com.gildedrose.SellIn;
import lombok.EqualsAndHashCode;
import lombok.Getter;

@Getter
@EqualsAndHashCode
public abstract class GildedRoseItem {

  private final String name;
  private final Quality quality;
  private final SellIn sellIn;
  private final boolean conjured;

  protected GildedRoseItem(Item item) {
    this(item.name, SellIn.create(item.sellIn), Quality.create(item.quality), false);
  }

  protected GildedRoseItem(String name, SellIn sellIn, Quality quality, boolean conjured) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
    this.conjured = conjured;
  }

  public void refreshState() {
    this.getSellIn().decrement();
    Quality nextQuality = this.nextQuality(this.getQuality());
    if (this.isConjured()) {
      nextQuality = this.nextQuality(nextQuality);
    }
    this.getQuality().setValue(nextQuality.getValue());
  }

  protected abstract Quality nextQuality(Quality previous);
}
