import CNST from "./util/constants";

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

  updateQuality() {
    for (let i = 0; i < this.items.length; i++) {
      const iterationItem = this.items[i];
      if (
        [CNST.AGED_BRIE, CNST.BCST_TAF].includes(iterationItem.name) &&
        iterationItem.quality > 0
      ) {
        iterationItem.quality = iterationItem.quality - 1;
      } else {
        if (iterationItem.quality < 50) {
          iterationItem.quality = iterationItem.quality + 1;
          if (iterationItem.name == CNST.BCST_TAF) {
            if (iterationItem.sellIn < 11) {
              iterationItem.quality = iterationItem.quality + 1;
            }
          }
        }
      }
      if (iterationItem.name != CNST.SULFURAS) {
        iterationItem.sellIn = iterationItem.sellIn - 1;
      }
      if (iterationItem.sellIn < 0) {
        if (iterationItem.name != CNST.AGED_BRIE) {
          if (iterationItem.name != CNST.BCST_TAF) {
            if (iterationItem.quality > 0) {
              if (iterationItem.name != CNST.SULFURAS) {
                iterationItem.quality = iterationItem.quality - 1;
              }
            }
          } else {
            iterationItem.quality =
              iterationItem.quality - iterationItem.quality;
          }
        } else {
          if (iterationItem.quality < 50) {
            iterationItem.quality = iterationItem.quality + 1;
          }
        }
      }
    }

    return this.items;
  }
}
