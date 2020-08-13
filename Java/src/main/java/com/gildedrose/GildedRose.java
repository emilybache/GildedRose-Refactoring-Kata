package com.gildedrose;

class GildedRose {
  Item[] items;

  public GildedRose(Item[] items) {
    this.items = items;
  }

  public void updateQuality() {

    for (Item item : items) {
      if (item.name.equals("Aged Brie")) {
        plusOneQuilaty_if_less_than_fifth(item);
      } else if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
        if (item.quality < 50) {
          item.quality = item.quality + 1;

          if (item.sellIn < 11) {
            plusOneQuilaty_if_less_than_fifth(item);
          }

          if (item.sellIn < 6) {
            plusOneQuilaty_if_less_than_fifth(item);
          }
        }
      } else {
        caculateQuilaty_without_Sulfuras(item);
      }

      if (item.name.equals("Sulfuras, Hand of Ragnaros")) {

      } else {
        item.sellIn = item.sellIn - 1;
      }

      if (item.sellIn < 0) {
        if (item.name.equals("Aged Brie")) {
          plusOneQuilaty_if_less_than_fifth(item);
        } else {
          if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            item.quality = item.quality - item.quality;
          } else {
            caculateQuilaty_without_Sulfuras(item);
          }
        }
      }
    }

  }

  private void plusOneQuilaty_if_less_than_fifth(Item item) {
    if (item.quality < 50) {
      item.quality = item.quality + 1;
    }
  }

  private void caculateQuilaty_without_Sulfuras(Item item) {
    if (item.quality > 0) {
      if (item.name.equals("Sulfuras, Hand of Ragnaros")) {

      } else {
        item.quality = item.quality - 1;
      }
    }
  }
}
