package com.gildedrose;

class GildedRose {
  Item[] items;

  public GildedRose(Item[] items) {
    this.items = items;
  }

  public void updateQuality() {

    for (Item item : items) {
      if (item.name.equals("Sulfuras, Hand of Ragnaros")) {

        return;
      } else if (item.name.equals("Aged Brie")) {

        increaseQuality(item);

        item.sellIn = item.sellIn - 1;

        if (item.sellIn < 0) {
          increaseQuality(item);
        }

      } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {

        if (item.quality < 50) {
          item.quality = item.quality + 1;

          if (item.sellIn < 11) {
            increaseQuality(item);
          }

          if (item.sellIn < 6) {
            increaseQuality(item);
          }
        }

        item.sellIn = item.sellIn - 1;

        if (item.sellIn < 0) {
          item.quality = item.quality - item.quality;
        }
      } else {

        decreaseQuality(item);
        item.sellIn = item.sellIn - 1;
        if (item.sellIn < 0) {
          decreaseQuality(item);
        }
      }
    }

  }

  private void increaseQuality(Item item) {
    if (item.quality < 50) {
      item.quality = item.quality + 1;
    }
  }

  private void decreaseQuality(Item item) {
    if (item.quality > 0) {
      item.quality = item.quality - 1;
    }
  }
}
