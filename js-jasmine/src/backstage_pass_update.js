function updateQuality(item) {
  if (is(item)) {
    item.quality += getQualityChange(item);
  }
}

function getQualityChange(item) {
  if (item.sellIn <= 0) {
    return -item.quality;
  } else if (item.sellIn <= 5) {
    return 3;
  } else if (item.sellIn <= 10) {
    return 2;
  } else {
    return 1;
  }
}

function is(item) {
  return item.name.toLowerCase().match(/backstage pass/);
};

module.exports = { updateQuality, is };