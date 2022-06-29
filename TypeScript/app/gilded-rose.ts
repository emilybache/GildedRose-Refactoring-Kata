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
      if (this.items[i].name != "Conjured Mana Cake") {
        if (this.items[i].name != 'Aged Brie' && this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
          if (this.items[i].quality > 0) {
            if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
              this.items[i].quality = this.items[i].quality - 1
            }
          }
        } else {
          if (this.items[i].quality < 50) {
            this.items[i].quality = this.items[i].quality + 1
            if (this.items[i].name == 'Backstage passes to a TAFKAL80ETC concert') {
              if (this.items[i].sellIn < 11) {
                if (this.items[i].quality < 50) {
                  this.items[i].quality = this.items[i].quality + 1
                }
              }
              if (this.items[i].sellIn < 6) {
                if (this.items[i].quality < 50) {
                  this.items[i].quality = this.items[i].quality + 1
                }
              }
            }
          }
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
      else {
        this.items[i].sellIn--;
         if(this.items[i].sellIn >= 0) {
           this.items[i].quality = this.items[i].quality - 2;
         }
         else {
           this.items[i].quality = this.items[i].quality - 4;
         }
         if(this.items[i].quality < 0) {
           this.items[i].quality = 0;
         }
      }
    }

    return this.items;
  }

  updateSellIn(i){
    if(this.items[i].name != 'Sulfuras, Hand of Ragnaros')
      this.items[i].sellIn--;
  }

  updateQualitySulfuras(i){
    this.items[i].quality = 80;
  }

  updateQualityAgedBrie(i){
    if(this.items[i].sellIn < 0) {
      this.items[i].quality = this.items[i].quality + 2;
    }
    else {
      this.items[i].quality = this.items[i].quality + 1;
    }
    this.items[i].quality = Math.min(this.items[i].quality,50)
  }

  updateQualityBackstage(i){
    if(this.items[i].sellIn < 0) {
      this.items[i].quality = 0;
    }
    else if(this.items[i].sellIn >= 10) {
      this.items[i].quality = this.items[i].quality + 1;
    }
    else if(this.items[i].sellIn >= 5) {
      this.items[i].quality = this.items[i].quality + 2;
    }
    else if(this.items[i].sellIn >= 0) {
      this.items[i].quality = this.items[i].quality + 3;
    }
    this.items[i].quality = Math.min(this.items[i].quality,50)
  }

  updateQualityConjured(i){
    if(this.items[i].sellIn < 0) {
      this.items[i].quality = this.items[i].quality - 4;
    }
    else {
      this.items[i].quality = this.items[i].quality - 2;
    }
    this.items[i].quality = Math.max(this.items[i].quality,0)
  }

  updateQualityAnythingElse(i){
    if(this.items[i].sellIn < 0) {
      this.items[i].quality = this.items[i].quality - 2;
    }
    else {
      this.items[i].quality = this.items[i].quality - 1;
    }
    this.items[i].quality = Math.max(this.items[i].quality,0)
  }

  updateQuality2(){
    for(let i = 0; i < this.items.length; i++) {
      this.updateSellIn(i);
      if(this.items[i].name == 'Sulfuras, Hand of Ragnaros') {
        this.updateQualitySulfuras(i)
      }
      else if(this.items[i].name == 'Aged Brie') {
        this.updateQualityAgedBrie(i);
      }
      else if(this.items[i].name == 'Backstage passes to a TAFKAL80ETC concert') {
        this.updateQualityBackstage(i);
      }
      else if(this.items[i].name == 'Conjured Mana Cake') {
        this.updateQualityConjured(i);
      }
      else{
        this.updateQualityAnythingElse(i);
      }
    }
    return this.items
  }
}
