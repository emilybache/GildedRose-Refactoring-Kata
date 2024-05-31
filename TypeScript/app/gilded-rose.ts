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

enum ItemNames {
  sulfuras = "Sulfuras, Hand of Ragnaros",
  agedBrie = "Aged Brie",
  backstagePasses = "Backstage passes to a TAFKAL80ETC concert",
  conjuredManCake = "Conjured Mana Cake",
}

export class GildedRose {
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  updateQuality() {
    for (let i = 0; i < this.items.length; i++) {
      let item = this.items[i];

      if (item.name == ItemNames.sulfuras) {
        item.quality = 80;
        continue;
      }

      // update sellIn logic
      item.sellIn -= 1;

      // update quality

      if (item.name == ItemNames.conjuredManCake) {
        item.quality -= 2;
        if (item.sellIn < 0) {
          item.quality -= 4;
        }
      } else if (item.name == ItemNames.agedBrie) {
        item.quality += 1;
      } else if (item.name == ItemNames.backstagePasses) {
        if (item.sellIn > 10) {
          item.quality += 1;
        } else if (item.sellIn > 5 && item.sellIn <= 10) {
          item.quality += 2;
        } else if (item.sellIn >= 0 && item.sellIn <= 5) {
          item.quality += 3;
        } else if (item.sellIn < 0) {
          item.quality = 0;
        }
      } else {
        // if the sellIn date has passed
        if (item.sellIn < 0) {
          item.quality -= 2;
        } else {
          // if it is within the sellin date
          item.quality -= 1;
        }
      }

      // quality can never be negative
      if (item.quality < 0) item.quality = 0;

      // has a ceiling of 50
      if (item.quality > 50) item.quality = 50;
    }

    return this.items;
  }
}
