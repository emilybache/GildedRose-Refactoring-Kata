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
    this.items.forEach((item)=> {
      if (['Aged Brie', 'Backstage passes to a TAFKAL80ETC concert'].indexOf(item.name) === -1 ) {
        if (item.quality > 0 && item.name !== 'Sulfuras, Hand of Ragnaros') {
            this.decreaseQuality(item);
        }
      } else {
        if (item.quality < 50) {
          this.increaseQuality(item)
          if (item.name === 'Backstage passes to a TAFKAL80ETC concert' &&
              (item.sellIn < 6 ||item.sellIn < 11) && item.quality < 50) {
                this.increaseQuality(item);
          }
        }
      }
      if (item.name !== 'Sulfuras, Hand of Ragnaros') {
        item.sellIn = item.sellIn - 1;
      }
      if (item.sellIn < 0) {
        if (item.name !== 'Aged Brie') {
          if (['Backstage passes to a TAFKAL80ETC concert', 'Sulfuras, Hand of Ragnaros'].indexOf(item.name) === -1 &&
              item.quality > 0) {
                this.decreaseQuality(item);
          } else {
            item.quality = item.quality - item.quality;
          }
        } else {
          if (item.quality < 50) {
            this.increaseQuality(item)
          }
        }
      }
    })
    return this.items;
  }

  increaseQuality(item) {
    item.quality = item.quality + 1;
  }

  decreaseQuality(item) {
    item.quality = item.quality - 1;
  }
}
module.exports = {
  Item,
  Shop
}
