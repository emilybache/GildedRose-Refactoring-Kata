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
      let currentItem = this.items[i];
      let itemName = currentItem.name;
      let itemQuality = currentItem.quality;
      let itemSellin = currentItem.sellIn;
      
      if (itemName.includes('Sulfuras')) {
        continue;
      } else {
        itemSellin -= 1;
      }

      switch (true) {
        case itemName.includes('Aged Brie'):
          itemQuality = itemQuality + 1;
          break;
        case itemName.includes('Backstage passes'):
          if (itemSellin < 11) {
            itemQuality = itemQuality + 2;
          } else if (itemSellin < 6) {
            itemQuality = itemQuality + 3;
          } else if (itemSellin < 0) {
            itemQuality = 0;
          } else {
            itemQuality = itemQuality + 1;
          }
          break;
        case itemName.includes('Conjured'):
          if (itemQuality > 0) {
            itemQuality = itemQuality - 2;
          }
          if (itemSellin < 0) {
            itemQuality = itemQuality - itemQuality * 2;
          }
          break;
        default:
          if (itemQuality > 0) {
            itemQuality = itemQuality - 1;
          }
          if (itemSellin < 0) {
            itemQuality = itemQuality - itemQuality;
          }
          break;
      }
      this.items[i].quality = itemQuality > 50 ? 50 : itemQuality;
      this.items[i].sellIn = itemSellin;
    }

    return this.items;
  }
}
