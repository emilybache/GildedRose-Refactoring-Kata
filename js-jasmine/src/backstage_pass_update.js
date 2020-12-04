const backstagePass = {
  regex_matcher: /backstage pass/,
  qualityChange: (sellIn, quality) => {
    if (sellIn <= 0) {
      return -quality;
    } else if (sellIn <= 5) {
      return 3;
    } else if (sellIn <= 10) {
      return 2;
    } else {
      return 1;
    }
  }
}

module.exports = { backstagePass };