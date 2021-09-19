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
  }

  updateQuality() {
    if (this.item.quality > 0 && this.item.quality <= 50) {
      this.item.quality = this.item.quality - 1;
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
