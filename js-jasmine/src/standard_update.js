var backstagePass = require('./backstage_pass_update.js')
var agedBrie = require('./aged_brie_update.js')
var sulfuras = require('./sulfuras_update.js')

function updateQuality(item) {
  if (is(item)) {
    item.quality += getQualityChange(item);
  }
}

function getQualityChange(item) {
  if (item.sellIn <= 0) {
    return -2;
  } else {
    return -1;
  }
}

function is(item) {
  return !(backstagePass.is(item) || agedBrie.is(item) || sulfuras.is(item))
};

module.exports = { updateQuality };