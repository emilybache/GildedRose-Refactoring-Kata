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
  updateQuality() {

    this.items.forEach(item => {

      // Set a degradation multiplier to 2 if expiration date has passed
      // Otherwise set it to 1 (no multiplier)
      let degradationMultiplier = item.sellIn < 0 ? 2 : 1

      switch (item.name) {
        case 'Aged Brie':
          item.quality++
          item.sellIn--
          break;
        case 'Backstage passes to a TAFKAL80ETC concert':
          switch (true) {
            case (item.sellIn < 0):
              item.quality = 0
              break;
            case (item.sellIn <= 5):
              item.quality += 3
              break;
            case (item.sellIn <= 10):
              item.quality += 2
              break;
            default:
              item.quality++
              break;
          }
          item.sellIn--
          break;
        case 'Sulfuras, Hand of Ragnaros':
          break;
        case 'Conjured Mana Cake':
          item.quality -= (2 * degradationMultiplier)
          item.sellIn--
          break;
        default:
          item.quality -= (1 * degradationMultiplier)
          item.sellIn--
      }

      // Item quality cannot be higher than 50 or lower than 0.
      if (item.quality > 50) item.quality = 50
      if (item.quality < 0) item.quality = 0

    })
      
    return this.items;
  }
}

module.exports = {
  Item,
  Shop
}
