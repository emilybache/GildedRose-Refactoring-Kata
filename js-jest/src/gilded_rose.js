class Item {
  constructor(name, sellIn, quality){
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

class Shop {
  constructor(items=[]){
    this.items = items;
  }
  updateQuality() {
    for (let i = 0; i < this.items.length; i++) {

      if(this.items[i].name !== 'Sulfuras') {
        if (this.items[i].name !== 'Aged Brie' && this.items[i].name !== 'Backstage passes') {
          if (this.items[i].sellIn <= 0) {
            if(this.items[i].name === 'Conjured' && this.items[i].quality - 4 > 0) {
              this.items[i].quality = this.items[i].quality - 4;
            } else if(this.items[i].quality - 2 > 0){
              this.items[i].quality = this.items[i].quality - 2;
            }
          }
        } else {
          if ((this.items[i].sellIn <= 10 && this.items[i].sellIn >5) && this.items[i].quality + 2 <= 50) {
            this.items[i].quality = this.items[i].quality + 2;
          } else if ((this.items[i].sellIn <= 5 && this.items[i].sellIn > 0) && this.items[i].quality + 3 <= 50) {
            this.items[i].quality = this.items[i].quality + 3;
          } else if(this.items[i].quality < 50) {
            this.items[i].quality = this.items[i].quality + 1;
          }
        }
      } else {
        if(this.items[i].quality < 80) {
          this.items[i].quality = 80;
        }
      }
    }
    return this.items;
  }
}

module.exports = {
  Item,
  Shop
}
