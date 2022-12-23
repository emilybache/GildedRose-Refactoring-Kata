export class Item {
  name: string;
  sellIn: number;
  quality: number;

  constructor(
    name: Item["name"],
    sellIn: Item["sellIn"],
    quality: Item["quality"]
  ) {
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
      const itemName = this.items[i].name;
      let itemQuality = this.items[i].quality;
      let itemSellin = this.items[i].sellIn;

      if (itemName.includes("Sulfuras")) {
        continue;
      } else {
        itemSellin -= 1;
      }

      if (itemName.includes("Aged Brie")) {
        itemQuality++;
      } else if (itemName.includes("Backstage passes")) {
        if (itemSellin < 11) {
          itemQuality = itemQuality + 2;
        } else if (itemSellin < 6) {
          itemQuality = itemQuality + 3;
        } else if (itemSellin < 0) {
          itemQuality = 0;
        } else {
          itemQuality++;
        }
      } else if (itemName.includes("Conjured")) {
        if (itemQuality > 0) {
          itemQuality = itemQuality - 2;
        }
        if (itemSellin < 0) {
          itemQuality = itemQuality - itemQuality * 2;
        }
      } else {
        if (itemQuality > 0) {
          itemQuality = itemQuality - 1;
        }

        if (itemSellin < 0) {
          itemQuality = 0;
        }
      }

      this.items[i].quality = itemQuality > 50 ? 50 : itemQuality;
      this.items[i].sellIn = itemSellin;
    }

    return this.items;
  }
}
