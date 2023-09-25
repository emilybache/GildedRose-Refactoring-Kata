import { ITEMS } from "./constants";
import { Item } from "./itemClasses";

export class GildedRose {
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  handleSellIn(item) {
    //
    if (item.name != ITEMS.SURFRAS) {
      item.sellIn -= 1;
    }
    if (item.sellIn >= 0) return;
    if (item.quality >= 50) return;

    //
    switch (item.name) {
      case ITEMS.BRIE:
        item.quality = item.quality + 1;
        break;
      case ITEMS.SURFRAS:
        item.quality = 0;
        break;
      default:
        item.quality -= 1;
        break;
    }
  }

  handlePassesQuality(item) {
    //
    if (item.name !== ITEMS.PASSES) return;
    if (6 <= item.sellIn && item.sellIn < 11) {
      item.quality += 1;
    }

    if (item.sellIn < 6) {
      item.quality += 2;
    }
  }

  handleQuality(item) {
    //
    switch (item.name) {
      case ITEMS.PASSES:
        this.handlePassesQuality(item);
        break;
      case ITEMS.SURFRAS:
        item.quality -= 1;
        break;
    }

    if (item.quality >= 50) return;
    item.quality += 1;
  }

  updateQuality() {
    for (const item of this.items) {
      if (!item.quality) break;
      this.handleSellIn(item);
      this.handleQuality(item);
    }

    return this.items;
  }
}
