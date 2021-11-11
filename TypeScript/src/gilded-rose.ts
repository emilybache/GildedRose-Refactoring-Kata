/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
export class Item {
  name: string
  sellIn: number
  quality: number

  constructor(name, sellIn, quality) {
    this.name = name
    this.sellIn = sellIn
    this.quality = quality
  }

  static maxQualityThreshold = 50
  static minQualityThreshold = 0
  static legendaryQuality = 80
}

export class GildedRose {
  items: Array<Item>

  constructor(items = [] as Array<Item>) {
    this.items = items
  }

  // est périmité ?
  isOutdated(item: Item) {
    return item.sellIn < 0
  }

  shouldDecreaseQuality(item: Item) {
    return item.name != 'Aged Brie' && item.name != 'Backstage passes to a TAFKAL80ETC concert'
  }

  isLegendayProduct(item: Item) {
    return item.name === 'Sulfuras, Hand of Ragnaros'
  }

  incrementQuality(item: Item) {
    if (item.quality < Item.maxQualityThreshold) {
      return item.quality + 1
    }
    return item.quality
  }

  decrementQuality(item: Item) {
    if (item.quality > Item.minQualityThreshold) {
      return item.quality - 1
    }
    return item.quality
  }

  updateQuality() {
    this.items.forEach(item => {
      if (this.isLegendayProduct(item)) {
        return
      }
      const currentProductName = item.name
      // PART 1
      if (this.shouldDecreaseQuality(item)) {
        item.quality = this.decrementQuality(item)
      } else {
        item.quality = this.incrementQuality(item)
        if (currentProductName === 'Backstage passes to a TAFKAL80ETC concert') {
          if (item.sellIn < 11) {
            item.quality = this.incrementQuality(item)
          }
          if (item.sellIn < 6) {
            item.quality = this.incrementQuality(item)
          }
        }
      }
      // part 2
      item.sellIn = item.sellIn - 1
      // part 3
      if (!this.isOutdated(item)) {
        return
      }

      if (currentProductName !== 'Aged Brie') {
        if (currentProductName !== 'Backstage passes to a TAFKAL80ETC concert') {
          item.quality = this.decrementQuality(item)
        } else {
          item.quality = item.quality - item.quality
        }
      } else {
        item.quality = this.incrementQuality(item)
      }
    })

    return this.items
  }
}
