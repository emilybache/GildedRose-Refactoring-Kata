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
      // Set a degradation multiplier to 2 if expiration date has passed
      // Otherwise set it to 1 (no multiplier)
      let degradationMultiplier = item.sellIn < 0 ? 2 : 1;

      this.items.forEach((item) => {
        switch (item.name) {
          case "Aged Brie":
            item.quality++;
            item.sellIn--;
            break;
          case "Backstage passes to a TAFKAL80ETC concert":
            switch (true) {
              case item.sellIn < 0:
                item.quality = 0;
                break;
              case item.sellIn <= 5:
                item.quality += 3;
                break;
              case item.sellIn <= 10:
                item.quality += 2;
                break;
              default:
                item.quality++;
                break;
            }
            itemSellIn--;
            item.sellIn--;
            break;
          case "Sulfuras, Hand of Ragnaros":
            break;
          case "Conjured Mana Cake":
            item.quality -= 2 * degradationMultiplier;
            item.sellIn--;
            break;
          default:
            item.quality -= 1 * degradationMultiplier;
            item.sellIn--;
        }

        // Item quality cannot be higher than 50 or lower than 0.
        if (itemQuality > 50) itemQuality = 50;
        if (itemQuality < 0) itemQuality = 0;

        // Use the modified variables to set the actual properties on the item

        if (item.quality > 50) item.quality = 50;
        if (item.quality < 0) item.quality = 0;
      });
    }

    return this.items;

    //   if (this.items[i].name != 'Aged Brie' && this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
    //     if (this.items[i].quality > 0) {
    //       if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
    //         this.items[i].quality = this.items[i].quality - 1;
    //       }
    //     }
    //   } else {
    //     if (this.items[i].quality < 50) {
    //       this.items[i].quality = this.items[i].quality + 1;
    //       if (this.items[i].name == 'Backstage passes to a TAFKAL80ETC concert') {
    //         if (this.items[i].sellIn < 11) {
    //           if (this.items[i].quality < 50) {
    //             this.items[i].quality = this.items[i].quality + 1;
    //           }
    //         }
    //         if (this.items[i].sellIn < 6) {
    //           if (this.items[i].quality < 50) {
    //             this.items[i].quality = this.items[i].quality + 1;
    //           }
    //         }
    //       }
    //     }
    //   }
    //   if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
    //     this.items[i].sellIn = this.items[i].sellIn - 1;
    //   }
    //   if (this.items[i].sellIn < 0) {
    //     if (this.items[i].name != 'Aged Brie') {
    //       if (this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
    //         if (this.items[i].quality > 0) {
    //           if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
    //             this.items[i].quality = this.items[i].quality - 1;
    //           }
    //         }
    //       } else {
    //         this.items[i].quality = this.items[i].quality - this.items[i].quality;
    //       }
    //     } else {
    //       if (this.items[i].quality < 50) {
    //         this.items[i].quality = this.items[i].quality + 1;
    //       }
    //     }
    //   }

    // return this.items;
  }
}
module.exports = {
  Item,
  Shop,
};
