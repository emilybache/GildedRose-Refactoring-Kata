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
    this.items.forEach(item => {

      // Define multiplier to 2 if expiration date has passed else set to 1
      const multiplier = item.sellIn < 0 ? 2 : 1

      switch (item.name) {
        case 'Aged Brie':
          item.quality++;
          item.sellIn--;
          break;
        case 'Backstage passes to a TAFKAL80ETC concert':
          switch (true) {
            case (item.sellIn < 0):
              item.quality = 0;
              break;
            case (item.sellIn <= 5):
              item.quality += 3;
              break;
            case (item.sellIn <= 10):
              item.quality += 2;
              break;
            default:
              item.quality++;
              break;
          }
          item.sellIn--;
          break;
        case 'Sulfuras, Hand of Ragnaros':
          break;
        case 'Conjured Mana Cake':
          item.quality -= (2 * multiplier);
          item.sellIn--;
          break;
        default:
          item.quality -= (1 * multiplier);
          item.sellIn--;
      }

      // Item quality cannot be greater than 50 or smaller than 0.
      if (item.quality > 50) item.quality = 50;
      if (item.quality < 0) item.quality = 0;

    });

    return this.items;
  }
}

module.exports = {
  Item,
  Shop
}