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

enum ItemType {
  AgedBrie = 'Aged Brie',
  BackstagePasses = 'Backstage passes to a TAFKAL80ETC concert',
  Sulfuras = 'Sulfuras, Hand of Ragnaros',
}

const TEN_DAYS_FOR_THE_CONCERT = 11;
const FIVE_DAYS_FOR_THE_CONCERT = 6;
const ZERO_DAYS_FOR_SALE = 0;
const MAX_QUALITY = 50;
const MIN_QUALITY = 0;

export class GildedRose {
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  updateQuality() {
    for (let i = 0; i < this.items.length; i++) {
      if (
        this.items[i].name != ItemType.AgedBrie &&
        this.items[i].name != ItemType.BackstagePasses
      ) {
        if (this.items[i].quality > MIN_QUALITY) {
          if (this.items[i].name != ItemType.Sulfuras) {
            this.items[i].quality = this.items[i].quality - 1;
          }
        }
      } else {
        if (this.items[i].quality < MAX_QUALITY) {
          this.items[i].quality = this.items[i].quality + 1;
          if (this.items[i].name == ItemType.BackstagePasses) {
            if (this.items[i].sellIn < TEN_DAYS_FOR_THE_CONCERT) {
              if (this.items[i].quality < MAX_QUALITY) {
                this.items[i].quality = this.items[i].quality + 1;
              }
            }
            if (this.items[i].sellIn < FIVE_DAYS_FOR_THE_CONCERT) {
              if (this.items[i].quality < MAX_QUALITY) {
                this.items[i].quality = this.items[i].quality + 1;
              }
            }
          }
        }
      }

      if (this.items[i].name != ItemType.Sulfuras) {
        this.items[i].sellIn = this.items[i].sellIn - 1;
      }

      if (this.items[i].sellIn < ZERO_DAYS_FOR_SALE) {
        if (this.items[i].name != ItemType.AgedBrie) {
          if (this.items[i].name != ItemType.BackstagePasses) {
            if (this.items[i].quality > MIN_QUALITY) {
              if (this.items[i].name != ItemType.Sulfuras) {
                this.items[i].quality = this.items[i].quality - 1;
              }
            }
          } else {
            this.items[i].quality =
              this.items[i].quality - this.items[i].quality;
          }
        } else {
          if (this.items[i].quality < MAX_QUALITY) {
            this.items[i].quality = this.items[i].quality + 1;
          }
        }
      }
    }

    return this.items;
  }
}
