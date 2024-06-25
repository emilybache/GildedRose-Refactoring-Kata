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

  isItemConjured(itemName: string): boolean {
    return itemName.startsWith("Conjured");
  }

  updateQuality() {
    for (let i = 0; i < this.items.length; i++) {

      // Process quality increase exceptions
      if (this.items[i].name == 'Aged Brie' || this.items[i].name == 'Backstage passes to a TAFKAL80ETC concert') {
        if (this.items[i].quality <= 49) {
          this.items[i].quality++;
          // Process additional potential quality increases for backstage passes
          if (this.items[i].name == 'Backstage passes to a TAFKAL80ETC concert') {
            if (this.items[i].sellIn <= 10 && this.items[i].quality <= 49) {
              this.items[i].quality++;
            }
            if (this.items[i].sellIn <= 5 && this.items[i].quality <= 49) {
              this.items[i].quality++;
            }
          }
        }
      } else {
        if (this.items[i].quality > 0) {
          // Decrease quality except in certain cases
          if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
            this.items[i].quality = this.items[i].quality - 1;
          }
        }
      }

      // Decrease 'sell in' value except in certain cases
      if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
        let expiryRate = 1;
        if(this.isItemConjured(this.items[i].name)) {
          expiryRate = 2;
        }
        this.items[i].sellIn -= expiryRate;
      }

      // Process additional quality change due to expiry
      if (this.items[i].sellIn < 0) {
        if (this.items[i].name != 'Aged Brie') {
          if (this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
            if (this.items[i].quality > 0) {
              if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
                this.items[i].quality--;
              }
            }
          // Process some exceptional cases
          } else {
            this.items[i].quality = 0;
          }
        } else {
          if (this.items[i].quality <= 49) {
            this.items[i].quality++;
          }
        }
      }
    }

    return this.items;
  }
}
