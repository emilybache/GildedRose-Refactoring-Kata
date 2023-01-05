class Item {
  constructor(name, sellIn, quality){
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}
// TODO: You just started using the switch statement and increment variables. Uncomment the bottom code and run the tests to see the last working version.
class Shop {
  constructor(items=[]){
    this.items = items;
  }
  updateQuality() {

    for (let i = 0; i < this.items.length; i++) {

      // Initialize three variables for easier reading below.
      let itemSellIn = this.items[i].sellIn
      let itemQuality = this.items[i].quality
      let itemName = this.items[i].name

      // Set a degradation multiplier to 2 if expiration date has passed
      // Otherwise set it to 1 (no multiplier)
      let degradationMultiplier = itemSellIn < 0 ? 2 : 1

      switch (itemName) {
        case 'Aged Brie':
          itemQuality++
          itemSellIn--
          break;
        case 'Backstage passes to a TAFKAL80ETC concert':
          let daysLeft = this.items[i].sellIn
          switch (true) {
            case (daysLeft < 0):
              itemQuality = 0
              break;
            case (daysLeft <= 5):
              itemQuality += 3
              break;
            case (daysLeft <= 10):
              itemQuality += 2
              break;
            default:
              itemQuality++
              break;
          }
          itemSellIn--
          break;
        case 'Sulfuras, Hand of Ragnaros':
          break;
        case 'Conjured Mana Cake':
          itemQuality -= (2 * degradationMultiplier)
          itemSellIn--
          break;
        default:
          itemQuality -= (1 * degradationMultiplier)
          itemSellIn--
      }

      // Item quality cannot be higher than 50 or lower than 0.
      if (itemQuality > 50) itemQuality = 50
      if (itemQuality < 0) itemQuality = 0

      // Use the modified variables to set the actual properties on the item
      this.items[i].sellIn = itemSellIn
      this.items[i].quality = itemQuality

    }
      
    return this.items;
  }
}

module.exports = {
  Item,
  Shop
}
