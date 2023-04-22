class Item {
  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

const itemsTypes = {
  BRIE: "Aged brie",
  PASS: "Backstage passes to a TAFKAL80ETC concert",
  HAND: "Sulfuras, Hand of Ragnaros",
  CON: "Conjured"
};

function updateBrie(item) {
  if (item.quality < 50) {
    item.quality = item.quality + 1;
  }

  item.sellIn = item.sellIn - 1;

  if (item.sellIn < 0 && item.quality < 50) {
    item.quality = item.quality + 1;
  }
};

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
};

function updateHand(item) {};

function updateDefault(item) {
  if (item.quality > 0) {
    item.quality = item.quality - 1;
  }

  item.sellIn = item.sellIn - 1;

  if (item.sellIn < 0 && item.quality > 0) {
    item.quality = item.quality - 1;
  }
};

function updateConjured(item) {
  if (item.quality > 0) {
    item.quality = item.quality - 2;
  }

  item.sellIn = item.sellIn - 1;

  if (item.sellIn < 0 && item.quality > 0) {
    item.quality = item.quality - 1;
  }
}

class Shop {
  constructor(items = []) {
    this.items = items;
  }

  updateQuality() {
    for (const item of this.items) {
      switch (item.name) {
        case itemsTypes.BRIE:
          updateBrie(item);
          continue;
        case itemsTypes.PASS:
          updatePass(item);
          continue;
        case itemsTypes.HAND:
          updateHand(item);
          continue;
        case itemsTypes.CON:
          updateConjured(item);
        default:
          updateDefault(item);
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
