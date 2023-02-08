import {
  updateQualityForAgedBrieItem,
  updateQualityForBackStageConcert,
  updateQualityForSulfurasItem,
  updateQualityForConjuredItem,
  updateQualityForNormalItem
} from './updateQuality';

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

  updateQuality(): Item[] {
    this.items.forEach(currentItem => {

      switch (currentItem.name) {
        case 'Aged Brie': {
          currentItem = updateQualityForAgedBrieItem(currentItem)
          break;
        }
        case 'Backstage passes to a TAFKAL80ETC concert': {
          currentItem = updateQualityForBackStageConcert(currentItem)
          break;
        }
        case 'Sulfuras, Hand of Ragnaros': {
          currentItem = updateQualityForSulfurasItem(currentItem)
          break;
        }
        case 'Conjured': {
          currentItem = updateQualityForConjuredItem(currentItem)
          break;
        }
        default: {
          currentItem = updateQualityForNormalItem(currentItem)
        }
      }
    });
    return this.items;
  }
}
