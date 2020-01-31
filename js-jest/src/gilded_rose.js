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

  isSulfuras(item) {
    return item.name == 'Sulfuras, Hand of Ragnaros';
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
      let qualityChange = 0;
      let sellInChange = -1;

      if (this.isSulfuras(item)) {
        sellInChange = 0;
      } else if(this.isAgedBrie(item)) {
        // Does conjured cheese quality increase twice as much?   Assume not, but
        // statement of problem doesn't say cheese quality doubles twice as fast after
        // the sell by date, but the code implements it that way.
        qualityChange = item.sellIn <= 0 ? 2 : 1;
      } else if (this.isConcertTicket(item)) {
        if(item.sellIn <= 0) {
          // This will set quality to 0
          qualityChange = -1 * item.quality;
        } else if(item.sellIn <= 5) {
          qualityChange = 3;
        } else if(item.sellIn <= 10) {
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

      item.sellIn = item.sellIn + sellInChange;
      item.quality = Math.min(Math.max(item.quality + qualityChange, 0), 50);
    }

    return this.items;
  }
}

module.exports = {
  Item,
  Shop
}
