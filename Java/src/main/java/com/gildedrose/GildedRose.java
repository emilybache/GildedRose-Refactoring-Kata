package com.gildedrose;

class GildedRose {
  Item[] items;

  public GildedRose(Item[] items) {
    this.items = items;
  }

  public void updateQuality() {

    for (int i = 0; i < items.length; i++) {
      if (items[i].name.equals("Aged Brie")) {
        extracted(i);
      } else if (items[i].name.equals("Backstage passes to a TAFKAL80ETC concert")) {
        if (items[i].quality < 50) {
          items[i].quality = items[i].quality + 1;

          if (items[i].sellIn < 11) {
            extracted(i);
          }

          if (items[i].sellIn < 6) {
            extracted(i);
          }
        }
      } else {
        caculateQuilaty_without_Sulfuras(i);
      }

      if (items[i].name.equals("Sulfuras, Hand of Ragnaros")) {

      } else {
        items[i].sellIn = items[i].sellIn - 1;
      }

      if (items[i].sellIn < 0) {
        if (items[i].name.equals("Aged Brie")) {
          extracted(i);
        } else {
          if (items[i].name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            items[i].quality = items[i].quality - items[i].quality;
          } else {
            caculateQuilaty_without_Sulfuras(i);
          }
        }
      }
    }
  }

  private void extracted(int i) {
    if (items[i].quality < 50) {
      items[i].quality = items[i].quality + 1;
    }
  }

  private void caculateQuilaty_without_Sulfuras(int i) {
    if (items[i].quality > 0) {
      if (items[i].name.equals("Sulfuras, Hand of Ragnaros")) {

      } else {
        items[i].quality = items[i].quality - 1;
      }
    }
  }
}
