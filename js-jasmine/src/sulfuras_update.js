function updateQuality(item) {
  if (is(item)) {
    item.quality += getQualityChange(item);
  }
}

function getQualityChange(item) {
  return 0;
}

function is(item) {
  return item.name.toLowerCase().match(/sulfuras/);
};

module.exports = { updateQuality, is };