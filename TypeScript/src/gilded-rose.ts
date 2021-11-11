/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
export class Item {
  name: string
  sellIn: number
  quality: number

  constructor(name: string, sellIn: number, quality: number) {
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

  isOutdated(item: Item) {
    return item.sellIn < 0
  }

  incrementQuality(item: Item) {
    return item.quality < Item.maxQualityThreshold ? item.quality + 1 : item.quality
  }

  decrementQuality(item: Item) {
    return item.quality > Item.minQualityThreshold ? item.quality - 1 : item.quality
  }

  updateQuality() {
    return this.items.map(item => {
      switch (item.name) {
        case 'Sulfuras, Hand of Ragnaros':
          this.updateLegendaryProduct(item)
          return item
        case 'Aged Brie':
          this.updateAgedBrie(item)
          return item
        case 'Backstage passes to a TAFKAL80ETC concert':
          this.updateBackstageProduct(item)
          return item
        default:
          this.updateNormalProduct(item)
          return item
      }
    })
  }

  private updateLegendaryProduct(item: Item) {
    item.quality = Item.legendaryQuality
  }

  private updateAgedBrie(item: Item) {
    //
    item.sellIn = item.sellIn - 1

    item.quality = this.incrementQuality(item)

    if (!this.isOutdated(item)) {
      return
    }

    item.quality = this.incrementQuality(item)

    return { ...item, sellIn: item.sellIn - 1, quality: 0 }
  }

  private updateBackstageProduct(item: Item) {
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
    }
  }

  private updateNormalProduct(item: Item) {
    item.sellIn = item.sellIn - 1
    item.quality = this.decrementQuality(item)
    if (this.isOutdated(item)) {
      item.quality = this.decrementQuality(item)
    }
  }
}
