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

  min(a: number, b: number) {
    return (a < b) ? a : b;
  }

  max(a: number, b: number) {
    return (a > b) ? a : b;
  }

  updateQuality() {
    for (let i = 0; i < this.items.length; i++) {
      if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
        this.items[i].sellIn--;
      }
      switch(this.items[i].name) {
        case 'Aged Brie':
          if (this.items[i].sellIn < 0) {
            this.items[i].quality += 2;
          } else {
            this.items[i].quality++;
          }
          break;
        case 'Backstage passes to a TAFKAL80ETC concert':
          if (this.items[i].sellIn > 10) {
            this.items[i].quality++;
          } else if (this.items[i].sellIn > 5) {
            this.items[i].quality += 2;
          } else if (this.items[i].sellIn > 0) {
            this.items[i].quality += 3;
          } else {
            this.items[i].quality = 0;
          }
          break;
        case 'Sulfuras, Hand of Ragnaros':
          break;
        case 'Conjured':
          if (this.items[i].sellIn > 0) {
            this.items[i].quality -= 2;
          } else {
            this.items[i].quality -= 4;
          }
          break;
        default:
          if (this.items[i].sellIn > 0) {
            this.items[i].quality--;
          } else {
            this.items[i].quality -= 2;
          }
          break;
      }

      this.items[i].quality = this.min(50, this.max(0, this.items[i].quality));
    }
    return this.items;
  }
}
