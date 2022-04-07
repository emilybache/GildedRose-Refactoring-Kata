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

      item.sellIn -= 1;

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
    let quality = item.quality;

    if (item.sellIn < 0) {
      quality += 2;
    } else {
      quality += 1;
    }

    item.quality = quality < 50 ? quality : 50;
  }
  updateBackstagePasses(item) {
    let quality = item.quality;

    if (item.sellIn < 0) {
      quality = 0;
    } else if (item.sellIn < 5) {
      quality += 3;
    } else if (item.sellIn < 10) {
      quality += 2;
    } else {
      quality += 1;
    }

    item.quality = quality < 50 ? quality : 50;
  }

  updateTheOthers(item) {
    let quality = item.quality;

    if (item.sellIn < 0) {
      quality -= 2;
    } else {
      quality -= 1;
    }

    item.quality = quality > 0 ? quality : 0;
  }
}

module.exports = {
  Item,
  Shop,
};
