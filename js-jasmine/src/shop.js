class Shop {
  constructor(items = []) {
    this.items = items;
  }
  updateQuality() {
    for (var i = 0; i < this.items.length; i++) {
      this._updateItemQuality(this.items[i]);
    }
    return this.items;
  }

  _updateItemQuality(item) {
    if (item.name != 'Aged Brie' && item.name != 'Backstage passes to a TAFKAL80ETC concert') {
      if (item.quality > 0) {
        if (item.name != 'Sulfuras, Hand of Ragnaros') {
          item.quality = item.quality - 1;
        }
      }
    } else {
      item.quality = item.quality + 1;
      if (item.name == 'Backstage passes to a TAFKAL80ETC concert') {
        if (item.sellIn < 11) {
          item.quality = item.quality + 1;
        }
        if (item.sellIn < 6) {
          item.quality = item.quality + 1;
        }
      }
    }
    this._updateSellIn(item)
    if (item.sellIn < 0) {
      if (item.name != 'Aged Brie') {
        if (item.name != 'Backstage passes to a TAFKAL80ETC concert') {
          if (item.quality > 0) {
            if (item.name != 'Sulfuras, Hand of Ragnaros') {
              item.quality = item.quality - 1;
            }
          }
        } else {
          item.quality = item.quality - item.quality;
        }
      } else {
        item.quality = item.quality + 1;
      }
    }
    this._checkMaxQuality(item);
  }

  _updateSellIn(item) {
    if (item.name != 'Sulfuras, Hand of Ragnaros') {
      item.sellIn = item.sellIn - 1;
    }
  }

  _checkMaxQuality(item) {
    if (item.quality > 50) {
      item.quality = 50;
    }
  }
}
module.exports = {
  Shop
}
