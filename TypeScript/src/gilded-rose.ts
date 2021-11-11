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
    return (
      item.name != 'Aged Brie' &&
      item.name != 'Backstage passes to a TAFKAL80ETC concert' &&
      item.quality > Item.minQualityThreshold
    )
  }

  isLegendayProduct(item: Item) {
    return item.name === 'Sulfuras, Hand of Ragnaros'
  }

  incrementQuality(item: Item) {
    return item.quality + 1
  }

  decrementQuality(item: Item) {
    return item.quality - 1
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
        if (item.quality < Item.maxQualityThreshold) {
          item.quality = this.incrementQuality(item)
          if (currentProductName == 'Backstage passes to a TAFKAL80ETC concert') {
            if (item.sellIn < 11) {
              if (item.quality < Item.maxQualityThreshold) {
                item.quality = this.incrementQuality(item)
              }
            }
            if (item.sellIn < 6) {
              if (item.quality < Item.maxQualityThreshold) {
                item.quality = this.incrementQuality(item)
              }
            }
          }
        }
      }
      // part 2
      item.sellIn = item.sellIn - 1
      // part 3
      if (this.isOutdated(item)) {
        if (currentProductName != 'Aged Brie') {
          if (currentProductName != 'Backstage passes to a TAFKAL80ETC concert') {
            if (item.quality > Item.minQualityThreshold) {
              item.quality = this.decrementQuality(item)
            }
          } else {
            item.quality = item.quality - item.quality
          }
        } else {
          if (item.quality < Item.maxQualityThreshold) {
            item.quality = this.incrementQuality(item)
          }
        }
      }
    })

    return this.items
  }
}
