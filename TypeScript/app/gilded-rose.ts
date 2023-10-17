import { log } from "console";

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
      // ITEM NAME CHECK
      if (this.items[i].name != 'Aged Brie' && this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
        if (this.items[i].quality > 0) {
          // Check if item name is not "Sulfuras, Hand of Ragnaros" & "Conjured Mana Cake"
          if (this.items[i].name != 'Sulfuras, Hand of Ragnaros' && this.items[i].name != 'Conjured Mana Cake') {
            // Decrease item quality by 1
            this.items[i].quality = this.items[i].quality - 1
          }
        }
      } else {
        // QUALITY CHECK
        if (this.items[i].quality < 50) {
          // Increase quality by 1 for 'Aged Brie'
          if (this.items[i].name == 'Aged Brie') {
            this.items[i].quality = this.items[i].quality + 1;
          } else {

            // If sellIn date for tickets is less than 11 & quality is less than 50 increase quality
            if (this.items[i].name == 'Backstage passes to a TAFKAL80ETC concert') {
              if (this.items[i].sellIn < 6) {
                this.items[i].quality = this.items[i].quality + 3
              } else if (this.items[i].sellIn < 11) {
                this.items[i].quality = this.items[i].quality + 2
              }
            }
          }
        }
        // Check if item name is "Conjured Mana Cake"
        if (this.items[i].name == 'Conjured Mana Cake') {
          this.items[i].quality = this.items[i].quality - 2;
        }
        if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
          this.items[i].sellIn = this.items[i].sellIn - 1;
        }
        if (this.items[i].sellIn < 0) {
          if (this.items[i].name != 'Aged Brie') {
            if (this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
              if (this.items[i].quality > 0) {
                if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
                  this.items[i].quality = this.items[i].quality - 1
                }
              }
            } else {
              this.items[i].quality = this.items[i].quality - this.items[i].quality
            }
          } else {
            if (this.items[i].quality < 50) {
              this.items[i].quality = this.items[i].quality + 1
            }
          }
        }
      }

    }
    console.log(this.items)
    return this.items;
  }

  checkSellInDate() {

  }


}
