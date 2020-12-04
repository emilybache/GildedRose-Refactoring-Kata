exports.regex_matcher = /conjured/;
exports.qualityChange = function (sellIn) {
  if (sellIn <= 0) {
    return -4;
  } else {
    return -2;
  }
};
