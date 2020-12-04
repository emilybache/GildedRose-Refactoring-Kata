const agedBrie = {
  regex_matcher: /aged brie/,
  qualityChange: (sellIn) => {
    if (sellIn <= 0) {
      return 2;
    } else {
      return 1;
    }
  }
}

module.exports = { agedBrie };