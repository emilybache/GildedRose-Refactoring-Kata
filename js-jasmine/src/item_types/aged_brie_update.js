exports.regex_matcher = /aged brie/;
exports.qualityChange = function (sellIn) {
  if (sellIn <= 0) {
    return 2;
  } else {
    return 1;
  }
};
