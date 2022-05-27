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

      if (!itemName.includes('Aged Brie') && !itemName.includes('Backstage passes')) {
        if (itemQuality > 0) {
          itemQuality = itemQuality - 1
        }
      } else {
        if (itemQuality < 50) {
          itemQuality = itemQuality + 1
          if (itemName.includes('Backstage passes')) {
            if (itemSellin < 11) {
              if (itemQuality < 50) {
                itemQuality = itemQuality + 1
              }
            }
            if (itemSellin < 6) {
              if (itemQuality < 50) {
                itemQuality = itemQuality + 1
              }
            }
          }
        }
      }
      if (itemSellin < 0) {
        if (!itemName.includes('Aged Brie')) {
          if (!itemName.includes('Backstage passes')) {
            if (itemQuality > 0) {
              itemQuality = itemQuality - 1
            }
          } else {
            itemQuality = itemQuality - itemQuality
          }
        } else {
          if (itemQuality < 50) {
            itemQuality = itemQuality + 1
          }
        }
      }
      this.items[i].quality = itemQuality;
      this.items[i].sellIn = itemSellin;
    }

    return this.items;
  }
}
