class Shop {
  constructor(items = []) {
    this.items = items;
    this.MAX_QUALITY = 50;
    this.MIN_QUALITY = 0;
  }
  updateQuality() {
    for (var i = 0; i < this.items.length; i++) {
      this._updateItemQuality(this.items[i]);
    }
    return this.items;
  }

  _updateItemQuality(item) {
    if (item.name == 'Backstage passes to a TAFKAL80ETC concert') {
      this._updateQualityBackstagePass(item)
    } else {
      if (item.name != 'Aged Brie') {
        if (item.name != 'Sulfuras, Hand of Ragnaros') {
          item.quality = item.quality - 1;
        }
      } else {
        item.quality = item.quality + 1;
      }
      this._updateSellIn(item)
      if (item.sellIn < 0) {
        if (item.name != 'Aged Brie') {
          if (item.name != 'Sulfuras, Hand of Ragnaros') {
            item.quality = item.quality - 1;
          }
        } else {
          item.quality = item.quality + 1;
        }
      }
    }
    this._checkMaxQuality(item);
    this._checkMinQuality(item);
  }

  _updateQualityBackstagePass(item) {
    if (item.sellIn <= 0) {
      item.quality = 0;
    } else if (item.sellIn < 6) {
      item.quality += 3;
    } else if (item.sellIn < 11) {
      item.quality += 2;
    } else {
      item.quality += 1;
    }
  }

  _updateSellIn(item) {
    if (item.name != 'Sulfuras, Hand of Ragnaros') {
      item.sellIn = item.sellIn - 1;
    }
  }

  _checkMaxQuality(item) {
    if (item.quality > this.MAX_QUALITY) {
      item.quality = this.MAX_QUALITY;
    }
  }

  _checkMinQuality(item) {
    if (item.quality < this.MIN_QUALITY) {
      item.quality = this.MIN_QUALITY;
    }
  }
}
module.exports = {
  Shop
}
