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
  increaseQuality(item) {
    if (item.quality < 50) {
      item.quality++;
    }
    return item;
  }
  decreaseQuality(item) {
    if (item.quality > 0) {
      if (item.sellIn < 0 || item.name === 'Conjured Mana Cake') {
        item.quality = item.quality - 2 > 0 ? item.quality - 2 : 0;
      } else {
        item.quality--;
      }
    }
    return item;
  }
  updateQuality() {
    this.items.map(item => {
      switch (item.name) {
        case 'Aged Brie':
          this.increaseQuality(item);
          item.sellIn--;
          break;
        case 'Backstage passes to a TAFKAL80ETC concert':
          if (item.sellIn > 0) {
            this.increaseQuality(item);
            if (item.sellIn < 11) {
              this.increaseQuality(item);
            }
            if (item.sellIn < 6) {
              this.increaseQuality(item);
            }
          } else {
            item.quality = 0;
          }
          item.sellIn--;
          break;
        case 'Sulfuras, Hand of Ragnaros':
          item.sellIn--;
          break;

        default:
          this.decreaseQuality(item);
          item.sellIn--;
      }
    });
    return this.items;
  }
}
