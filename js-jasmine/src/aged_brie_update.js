function updateQuality(item) {
  if (is(item)) {
    item.quality += getQualityChange(item);
  }
}

function getQualityChange(item) {
  if (item.sellIn <= 0) {
    return 2;
  } else {
    return 1;
  }
}

function is(item) {
  return item.name.toLowerCase().match(/aged brie/);
};

module.exports = { updateQuality, is };