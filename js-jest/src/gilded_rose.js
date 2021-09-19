class Item {
  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
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
  Shop
}
