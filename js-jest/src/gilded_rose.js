class Item {
  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

const itemType = {
  PASS: "Backstage passes to a TAFKAL80ETC concert",
  BRIE: "Aged Brie",
  SULFURAS: "Sulfuras, Hand of Ragnaros",
  CONJURED: "Conjured Mana Cake",
};

function updateBrie(item) {
  if (item.quality < 50) {
    item.quality = item.quality + 1;
  }
  item.sellIn = item.sellIn - 1;
  if (item.sellIn < 0 && item.quality < 50) {
    item.quality = item.quality + 1;
  }
  lessAndGreaterThan(item);
}
function updateHand(item) {}
function updateNormal(item) {
  item.sellIn = item.sellIn - 1;
  if (item.quality > 0) {
    item.quality = item.quality - 1;
  }

  if (item.sellIn < 0 && item.quality > 0) {
    item.quality = item.quality - 1;
  }
  lessAndGreaterThan(item);
}

function updatePass(item) {
  if (item.quality < 50) {
    item.quality = item.quality + 1;
    if (item.sellIn < 11 && item.quality < 50) {
      item.quality = item.quality + 1;
    }
    if (item.sellIn < 6 && item.quality < 50) {
      item.quality = item.quality + 1;
    }
  }
  item.sellIn = item.sellIn - 1;
  if (item.sellIn < 0) {
    item.quality = item.quality - item.quality;
  }
  lessAndGreaterThan(item);
}

const lessAndGreaterThan = (item) => {
  if (item.quality > 50) item.quality = 50;
  if (item.quality < 0) item.quality = 0;
};

class Shop {
  constructor(items = []) {
    this.items = items;
  }

  updateQuality() {
    for (const item of this.items) {
      // Set a degradation multiplier to 2 if expiration date has passed
      // Otherwise set it to 1 (no multiplier)

      let degradationMultiplier = item.sellIn < 0 ? 2 : 1;

      switch (item.name) {
        case itemType.BRIE:
          updateBrie(item);
          continue;
        case itemType.PASS:
          updatePass(item);
          continue;
        case itemType.SULFURAS:
          continue;
        case itemType.CONJURED:
          item.quality -= 2 * degradationMultiplier;
          item.sellIn--;
          lessAndGreaterThan(item);
          continue;
        default:
          item.quality -= 1 * degradationMultiplier;
          item.sellIn--;
          lessAndGreaterThan(item);
          continue;
      }
    }

    return this.items;
  }
}
module.exports = {
  Item,
  Shop,
};
