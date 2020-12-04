const standardItem = {
  qualityChange: (sellIn) => {
    if (sellIn <= 0) {
      return -2;
    } else {
      return -1;
    }
  }
}

module.exports = { standardItem };