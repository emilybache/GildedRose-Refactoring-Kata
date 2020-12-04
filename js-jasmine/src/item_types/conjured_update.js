exports.regex_matcher = /conjured/;
exports.qualityChange = function (sellIn, quality) {
  if (sellIn <= 0) {
    return -4;
  } else {
    return -2;
  }
};
