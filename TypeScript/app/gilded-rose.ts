import { ITEMS } from "./constants";

export class Item {
  name: string;
  sellIn: number;
  quality: number;

  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

export class GildedRose {
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  handlePassesQuality(item) {
    if (item.name !== ITEMS.PASSES) return;
    if (6 <= item.sellIn && item.sellIn < 11) {
      item.quality += 1;
    }

    if (item.sellIn < 6) {
      item.quality += 2;
    }
  }

  handleIfSellInIs0(item) {
    if (item.sellIn >= 0) return;

    switch (item.name) {
      case ITEMS.BRIE:
        if (item.quality >= 50) return;
        item.quality = item.quality + 1;
        break;
      case ITEMS.SURFRAS:
        item.quality = 0;
        break;
      default:
        if (!item.quality) return;
        item.quality -= 1;
        break;
    }
  }

  updateQuality() {
    for (const item of this.items) {
      if (!item.quality) break;
      if (item.name != ITEMS.SURFRAS) {
        item.sellIn -= 1;
      }
      this.handleIfSellInIs0(item);

      if (item.name != ITEMS.BRIE && item.name != ITEMS.PASSES) {
        if (item.name === ITEMS.SURFRAS) break;
        item.quality -= 1;
      }

      if (item.quality >= 50) break;
      item.quality += 1;

      this.handlePassesQuality(item);
    }

    return this.items;
  }
}
