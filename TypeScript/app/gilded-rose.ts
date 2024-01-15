import { assert } from "console";
import { ItemNames, MAX_ITEM_QUALITY } from "./constants";

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

  private updateAgedBrieItem(item: Item) {
    assert(item.name == ItemNames.AGED_BRIE);
    if (item.quality < MAX_ITEM_QUALITY) {
      item.quality = item.quality + 1;
    }
    if (item.name != ItemNames.SULFURAS) {
      item.sellIn = item.sellIn - 1;
    }
    if (item.sellIn < 0 && item.quality < MAX_ITEM_QUALITY) {
      item.quality = item.quality + 1;
    }
  }

  private updateBackstagePassesItem(item: Item) {
    assert(item.name == ItemNames.BACKSTAGE_PASSES);
    if (item.quality < MAX_ITEM_QUALITY) {
      item.quality = item.quality + 1;
      if (item.name == ItemNames.BACKSTAGE_PASSES && item.sellIn < 11) {
        if (item.sellIn < 11 && item.quality < MAX_ITEM_QUALITY) {
          item.quality = item.quality + 1;
        }
        if (item.sellIn < 6 && item.quality < MAX_ITEM_QUALITY) {
          item.quality = item.quality + 1;
        }
      }
    }
    if (item.name != ItemNames.SULFURAS) {
      item.sellIn = item.sellIn - 1;
    }
    if (item.sellIn < 0) {
      item.quality = 0;
    }
  }

  private updateSulfurasItem(item: Item) {
    assert(item.name == ItemNames.SULFURAS);
    // this function does not update the item (tests are still passing without any logic in this function)
  }

  private updateNormalItem(item: Item) {
    assert(
      item.name != ItemNames.AGED_BRIE &&
        item.name != ItemNames.BACKSTAGE_PASSES &&
        item.name != ItemNames.SULFURAS
    );
    if (item.quality > 0) {
      item.quality = item.quality - 1;
    }
    item.sellIn = item.sellIn - 1;
    if (item.sellIn < 0 && item.quality > 0) {
      item.quality = item.quality - 1;
    }
  }

  private updateConjuredItem(item: Item) {
    assert(item.name == ItemNames.CONJURED_CAKE);
    //business logic to be implemented
  }

  updateQuality() {
    for (const item of this.items) {
      switch (item.name) {
        case ItemNames.AGED_BRIE:
          this.updateAgedBrieItem(item);
          continue;
        case ItemNames.BACKSTAGE_PASSES:
          this.updateBackstagePassesItem(item);
          continue;
        case ItemNames.SULFURAS:
          this.updateSulfurasItem(item);
          continue;
        default:
          this.updateNormalItem(item);
          continue;
      }
    }

    return this.items;
  }
}
