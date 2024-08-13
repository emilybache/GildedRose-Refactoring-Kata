enum ItemNames {
  AgedBrie = 'Aged Brie',
  BackstagePasses = 'Backstage passes to a TAFKAL80ETC concert',
  Sulfuras = 'Sulfuras, Hand of Ragnaros'
}

const itemNames = [ItemNames.AgedBrie, ItemNames.BackstagePasses, ItemNames.Sulfuras];

export class Item {
  name: string | ItemNames;
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
      const item = this.items[i];
      if (!itemNames.includes(item.name as ItemNames) && item.quality > 0) {
        item.quality = item.quality - 1
      } else if (item.quality < 50) {
        item.quality = item.quality + 1
        if (item.name == ItemNames.BackstagePasses && item.sellIn < 11) {
          item.quality = item.quality + 1
        }
      }
      if (item.name != ItemNames.Sulfuras) {
        item.sellIn = item.sellIn - 1;
      }
      if (item.sellIn < 0) {
        if (item.name != ItemNames.AgedBrie) {
          const shouldDecreaseQuality = ![ItemNames.BackstagePasses, ItemNames.Sulfuras].includes(item.name as ItemNames) && item.quality > 0;
          item.quality = shouldDecreaseQuality ? item.quality - 1 : 0;
        } else {
          if (item.quality < 50) {
            item.quality = item.quality + 1
          }
        }
      }
    }

    return this.items;
  }
}
