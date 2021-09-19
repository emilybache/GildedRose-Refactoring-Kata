class Item {
  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

class ItemUpdater {
  constructor(item) {
    this.item = item;
    this.qualityChangeFactor = 1;
  }

  updateQuality() {

    //Once the sell by date has passed, Quality degrades twice as fast
    if(this.item.sellIn < 0) {
      this.qualityChangeFactor = 2;
    }

    //The Quality of an item is never more than 50
    if (this.item.quality > 50) {
      this.item.quality = 50;
    }

    if (this.item.quality > 0 && this.item.quality < 50) {
      this.item.quality = this.item.quality - this.qualityChangeFactor;
    }
    return this.item;
  }
}

class Shop {
  constructor(itemUpdaters=[]){
    this.itemUpdaters = itemUpdaters;
  }

  updateQuality() {
    let items = [];

    for (let i = 0; i < this.itemUpdaters.length; i++) {
      let item = this.itemUpdaters[i].updateQuality();
      items.push(item);
    }

    return items;
  }
}

module.exports = {
  Item,
  Shop,
  ItemUpdater
}
