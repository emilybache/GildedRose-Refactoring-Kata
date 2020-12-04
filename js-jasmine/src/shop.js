const standardItem = require('./standard_update.js')
const backstagePass = require('./backstage_pass_update.js')
const agedBrie = require('./aged_brie_update.js')
const sulfuras = require('./sulfuras_update.js')

const itemTypes = [backstagePass, agedBrie, sulfuras]

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
    this._updateItemSellIn(item);
  }

  _updateItemQuality(item) {
    this._conditionallyUpdateQuality(item)
    this._checkQualityBounds(item)
  }

  _conditionallyUpdateQuality(item) {
    for (const itemType of itemTypes) {
      if (item.name.toLowerCase().match(itemType.regex_matcher)) {
        return item.quality += itemType.qualityChange(item.sellIn, item.quality);
      }
    }
    return item.quality += standardItem.qualityChange(item.sellIn);
  }

  _updateItemSellIn(item) {
    if (item.name != 'Sulfuras, Hand of Ragnaros') {
      item.sellIn = item.sellIn - 1;
    }
  }

  _checkQualityBounds(item) {
    this._checkMaxQuality(item);
    this._checkMinQuality(item);
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
