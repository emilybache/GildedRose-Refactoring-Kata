class Item {
  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

class Shop {
  constructor(items = []) {
    this.items = items;
  }

  updateQuality() {
    for (let i = 0; i < this.items.length; i++) {
      const item = this.items[i];
      if (item.name === "Sulfuras, Hand of Ragnaros") continue;

      item.sellIn = item.sellIn - 1;
      switch (item.name) {
        case "Aged Brie":
          this.updateAgedBrie(item);
          break;
        case "Backstage passes to a TAFKAL80ETC concert":
          this.updateBackstagePasses(item);
          break;
        default:
          this.updateTheOthers(item);
          break;
      }
    }

    return this.items;
  }
  updateAgedBrie(item) {
    if (item.quality < 50) {
      item.quality = item.quality + 1;

      if (item.sellIn < 0) {
        item.quality = item.quality + 1;
      }
    }
  }
  updateBackstagePasses(item) {
    if (item.quality < 50) {
      item.quality = item.quality + 1;

      if (item.sellIn < 10 && item.quality < 50) {
        item.quality = item.quality + 1;
      }
      if (item.sellIn < 5 && item.quality < 50) {
        item.quality = item.quality + 1;
      }
    }

    if (item.sellIn < 0) {
      item.quality = item.quality - item.quality;
    }
  }

  updateTheOthers(item) {
    if (item.quality > 0) {
      item.quality = item.quality - 1;

      if (item.sellIn < 0) {
        item.quality = item.quality - 1;
      }
    }
  }
}

module.exports = {
  Item,
  Shop,
};
