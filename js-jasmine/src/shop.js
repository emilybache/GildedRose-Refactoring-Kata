class Shop {
  constructor(items = []) {
    this.items = items;
    this.MAX_QUALITY = 50;
    this.MIN_QUALITY = 0;
  }
  updateQuality() {
    this.items.forEach(item =>
      this._updateItem(item)
    )
    return this.items;
  }

  _updateItem(item) {
    this._updateItemQuality(item);
    this._updateItemSellIn(item)
    this._checkMaxQuality(item);
    this._checkMinQuality(item);
  }

  _updateItemQuality(item) {
    if (this._isBackstagePass(item)) {
      this._updateQualityBackstagePass(item);
    } else if (this._isAgedBrie(item)) {
      this._updateQualityAgedBrie(item);
    } else if (this._isSulfuras(item)) {
    } else {
      this._updateQualityStandard(item)
    }
  }

  _isBackstagePass(item) {
    return item.name.toLowerCase().match(/backstage pass/);
  }

  _isAgedBrie(item) {
    return item.name.toLowerCase().match(/aged brie/);
  }

  _isSulfuras(item) {
    return item.name.toLowerCase().match(/sulfuras/);
  }

  _updateQualityStandard(item) {
    if (item.sellIn <= 0) {
      item.quality -= 2;
    } else {
      item.quality -= 1;
    }
  }

  _updateQualityBackstagePass(item) {
    if (item.sellIn <= 0) {
      item.quality = 0;
    } else if (item.sellIn <= 5) {
      item.quality += 3;
    } else if (item.sellIn <= 10) {
      item.quality += 2;
    } else {
      item.quality += 1;
    }
  }

  _updateQualityAgedBrie(item) {
    if (item.sellIn <= 0) {
      item.quality += 2;
    } else {
      item.quality += 1;
    }
  }

  _updateItemSellIn(item) {
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
