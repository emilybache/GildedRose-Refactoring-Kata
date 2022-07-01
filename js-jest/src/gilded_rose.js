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

  get minQuality() {
    return 0;
  }

  get maxQuality() {
    return 50;
  }

  checkQuality({quality}) {
    const {minQuality, maxQuality} = this;

    return quality - 1 < minQuality || quality + 1 > maxQuality;
  }

  updateItemQuality(item) {
    if (this.checkQuality(item)) {
      return;
    }

    if (item.sellIn < 0) {
      if (item.name != 'Aged Brie') {
        if (item.name != 'Backstage passes to a TAFKAL80ETC concert') {
          if (item.name != 'Sulfuras, Hand of Ragnaros') {
            item.quality = item.quality - 1;
          }
        }
      } else {
        item.quality = item.quality + 1;
      }
    }

    if (item.name != 'Aged Brie' && item.name != 'Backstage passes to a TAFKAL80ETC concert') {
      if (item.name != 'Sulfuras, Hand of Ragnaros') {
        item.quality = item.quality - 1;
      }
    } else {
      if (item.name == 'Backstage passes to a TAFKAL80ETC concert') {
        if (item.sellIn < 10 && item.sellIn > 5) {
          item.quality = item.quality + 2;
        }
        else if (item.sellIn >= 0 && item.sellIn <= 5) {
          item.quality = item.quality + 3;
        }
        else if (item.sellIn > 0) {
          item.quality = item.quality + 1;
        } else {
          item.quality = 0;
        }
      } else {
        item.quality = item.quality + 1;
      }
    }
  }

  updateItemSellIn(item) {
    if (item.name != 'Sulfuras, Hand of Ragnaros') {
      item.sellIn = item.sellIn - 1;
    }
  }

  updateQuality() {
    this.items.forEach(item => {
      this.updateItemSellIn(item);
      this.updateItemQuality(item);
    });

    return this.items;
  }
}

module.exports = {
  Item,
  Shop
}
