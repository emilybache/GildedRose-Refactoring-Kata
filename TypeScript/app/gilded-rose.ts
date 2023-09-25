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

  handleIfSellInIs0(item) {
    if (item.sellIn >= 0) return;

    switch (item.name) {
      case ITEMS.BRIE:
        if (item.quality >= 50) return;
        item.quality = item.quality + 1;
        break;
      case ITEMS.SURFRAS:
        item.quality = item.quality - item.quality;
        break;
      default:
        if (!item.quality) return;
        item.quality -= 1;
        break;
    }
  }

  updateQuality() {
    for (const item of this.items) {
      if (item.name != ITEMS.BRIE && item.name != ITEMS.PASSES) {
        if (!item.quality) break;
        if (item.name != ITEMS.SURFRAS) {
          item.quality -= 1;
        }
      } else {
        if (item.quality < 50) {
          item.quality += 1;

          if (item.name == ITEMS.PASSES) {
            if (item.sellIn < 11) {
              if (item.quality < 50) {
                item.quality += 1;
              }
            }

            if (item.sellIn < 6) {
              if (item.quality >= 50) break;
              item.quality += 1;
            }
          }
        }
      }
      if (item.name != ITEMS.SURFRAS) {
        item.sellIn -= 1;
      }
      this.handleIfSellInIs0(item);
    }

    return this.items;
  }
}
