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

  isNormalProduct(item: Item) {
    return (
      item.name !== 'Aged Brie' &&
      item.name !== 'Backstage passes to a TAFKAL80ETC concert' &&
      item.name !== 'Sulfuras, Hand of Ragnaros'
    )
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
    if (item.quality <= Item.minQualityThreshold) {
      return item.quality
    }
    return item.quality - 1
  }

  updateQuality() {
    this.items.forEach(item => {
      //LEGENDARY
      if (this.isLegendayProduct(item)) {
        item.quality = 80
        return
      }
      const currentProductName = item.name

      // NORMAL PRODUCT
      if (this.isNormalProduct(item)) {
        item.sellIn = item.sellIn - 1

        if (this.isOutdated(item)) {
          item.quality = this.decrementQuality(item)
          item.quality = this.decrementQuality(item)
          return
        }

        item.quality = this.decrementQuality(item)
        return
      }

      // BACKSTAGE
      if (currentProductName === 'Backstage passes to a TAFKAL80ETC concert') {
        item.quality = this.incrementQuality(item)

        if (item.sellIn < 11) {
          item.quality = this.incrementQuality(item)
        }

        if (item.sellIn < 6) {
          item.quality = this.incrementQuality(item)
        }

        item.sellIn = item.sellIn - 1

        if (item.sellIn <= 0) {
          item.quality = 0
          return
        }

        return
      }

      // AGED BRIE
      if (currentProductName === 'Aged Brie') {
        item.quality = this.incrementQuality(item)
        item.sellIn = item.sellIn - 1
        if (!this.isOutdated(item)) {
          return
        }

        item.quality = this.incrementQuality(item)
        return
      }
    })

    return this.items
  }
}
