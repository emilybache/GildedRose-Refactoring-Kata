import { Item } from "./components/Item";
import { AgedBrie } from "./components/aged-brie";
import { BackstagePass } from "./components/backstagepass";
import { Conjured } from "./components/conjured";
import { Sulfuras } from "./components/sulfuras";
import { ItemConstants } from "./constants/constant";


export class GildedRose {
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  /**
   * Function to create item
   * @param name 
   * @param sellIn 
   * @param quality 
   * @returns 
   */
  static createItem(name: string, sellIn: number, quality: number): Item {
    switch (name) {
      case ItemConstants.AGED_BRIE:
        return new AgedBrie(name, sellIn, quality);
      case ItemConstants.BACKSTAGE:
        return new BackstagePass(name, sellIn, quality);
      case ItemConstants.SULFURAS:
        return new Sulfuras();
      case ItemConstants.CONJURED:
        return new Conjured(name, sellIn, quality);
      default:
        return new Item(name, sellIn, quality);
    }
  }

  /**
   * Function to update quality
   */
  updateQuality() {
    for (const item of this.items) {
      item.updateQuality();
    }

    return this.items;
  }
}
