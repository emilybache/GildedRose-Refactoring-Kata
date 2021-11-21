class Item {
  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}
const rules = [
  {
    pattern: /^Aged Brie/,
    quality: (itemQuality, itemSellIn) => itemQuality + 1,
    sellIn: (itemQuality, itemSellIn) => itemSellIn - 1,
  },
  {
    pattern: /^Sulfuras/,
    quality: (itemQuality, itemSellIn) => itemQuality,
    sellIn: (itemQuality, itemSellIn) => itemSellIn,
  },
  {
    pattern: /^Backstage passes/,
    quality: (itemQuality, itemSellIn) => {
      if (itemSellIn <= 0) {
        return 0;
      } else if (itemSellIn < 6) {
        return itemQuality + 3;
      } else if (itemSellIn < 11) {
        return itemQuality + 2;
      } else {
        return itemQuality + 1;
      }
    },
    sellIn: (itemQuality, itemSellIn) => itemSellIn - 1,
  },
  {
    pattern: /^Conjured/,
    quality: (itemQuality, itemSellIn) => itemQuality - 2,
    sellIn: (itemQuality, itemSellIn) => itemSellIn - 1,
  },
  {
    pattern: "*",
    quality: (itemQuality, itemSellIn) => itemQuality - 1,
    sellIn: (itemQuality, itemSellIn) => itemSellIn - 1,
  },
];
class Shop {
  constructor(items = []) {
    this.items = items;
  }
  updateQuality() {
    this.items = this.items.map((item) => {
      for (let i = 0; i < rules.length; i++) {
        const rule = rules[i];
        if (rule.pattern === "*" || item.name.match(rule.pattern)) {
          item.sellIn = rule.sellIn(item.quality, item.sellIn);
          item.quality = rule.quality(item.quality, item.sellIn);
          break;
        }
      }
      return item;
    });
    return this.items;
  }
}

module.exports = {
  Item,
  Shop,
};
