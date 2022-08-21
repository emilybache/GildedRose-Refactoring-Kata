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
        if (this.items[i].quality > 0) {
            this.items[i].quality = this.items[i].quality - 1;
        }

        this.items[i].sellIn = this.items[i].sellIn - 1;
    }

    return this.items;
  }
}

module.exports = {
  Item,
  Shop
}
