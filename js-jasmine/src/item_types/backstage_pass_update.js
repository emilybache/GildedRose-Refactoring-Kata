exports.regex_matcher = /backstage pass/;
exports.qualityChange = function (sellIn, quality) {
  if (sellIn <= 0) {
    return -quality;
  } else if (sellIn <= 5) {
    return 3;
  } else if (sellIn <= 10) {
    return 2;
  } else {
    return 1;
  }
};
