class Item {
  constructor(name, sellIn, quality){
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

class Shop {
  constructor(items=[]){
    this.items = items;
  }

  // Should really be method on Item, but I don't want to offend the goblins.
  isAgedBrie(item) {
    return item.name.startsWith('Aged Brie');
  }

  // Should really be method on Item, but I don't want to offend the goblins.
  isConcertTicket(item) {
    return item.name.startsWith('Backstage passes to ');
  }

  // Should really be method on Item, but I don't want to offend the goblins.
  isConjured(item) {
    return item.name.startsWith('Conjured ');
  }

  updateQuality() {
    for (let item of this.items) {
      if (item.name == 'Sulfuras, Hand of Ragnaros') {
        continue;
      }

      let qualityChange = 0;
      if(this.isAgedBrie(item)) {
        // Does conjured cheese quality increase twice as much?   Assume not, but
        // statement of problem doesn't say cheese quality doubles twice as fast after
        // the sell by date, but the code implements it that way.
        qualityChange = item.sellIn <= 0 ? 2 : 1;
      } else if (this.isConcertTicket(item)) {
        if(item.sellIn <= 0) {
          qualityChange = -1 * item.quality;
        } else if(item.sellIn < 6) {
          qualityChange = 3;
        } else if(item.sellIn < 11) {
          qualityChange = 2;
        } else {
          qualityChange = 1;
        }
      } else {
        qualityChange = item.sellIn <= 0 ? -2 : -1;
        if(this.isConjured(item)) {
          qualityChange = qualityChange * 2;
        }
      }

      item.sellIn--;
      item.quality = Math.min(Math.max(item.quality + qualityChange, 0), 50);
    }

    return this.items;
  }
}

module.exports = {
  Item,
  Shop
}
