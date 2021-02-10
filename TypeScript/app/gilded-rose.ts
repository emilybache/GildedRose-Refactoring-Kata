export class Item {
  name: string
  sellIn: number
  quality: number

  constructor(name, sellIn, quality) {
    this.name = name
    this.sellIn = sellIn
    this.quality = quality
  }
}

export const LegendaryItemList = [
    'Sulfuras, Hand of Ragnaros'
]

export const QualityIncreaseList = [
    'Backstage passes to a TAFKAL80ETC concert'
]

export const QualityIncreaseAtHigherRateList = [
  'Backstage passes to a TAFKAL80ETC concert'
]

export const QualityDecreaseAtHigherRateList = [
    'Conjured'
]

export const QualityIncreaseAfterSellInList = [
    'Aged Brie'
]

export const QualityBecomesZeroAfterSellIn = [
  'Backstage passes to a TAFKAL80ETC concert'
]

export const ItemNames = [
  'Backstage passes to a TAFKAL80ETC concert',
  'Aged Brie',
  'Conjured',
  'Sulfuras, Hand of Ragnaros'
]

export class GildedRose {
  items: Array<Item>

  constructor(items = [] as Array<Item>) {
    this.items = items
  }

  updateQuality() {
    let legendaryItems: Array<Item>
    legendaryItems = this.items.filter((item: Item): boolean => !this.shouldQualityChange(item))
    this.items = this.items.filter((item: Item): boolean => this.shouldQualityChange(item))

    for (const item of this.items) {
      if (ItemNames.indexOf(item.name) !== -1) {
        item.sellIn--
        if (this.shouldQualityIncrease(item)) {
          this.increaseQuality(item)
        } else {
          this.decreaseQuality(item)
        }
      } else {
        item.name = 'fixme'
      }
    }

    return legendaryItems.concat(this.items)
  }

  private decreaseQuality(item: Item): void {
    let amountToDecrease = 1
    if (item.sellIn < 0) {
      amountToDecrease = amountToDecrease * 2
    }
    if (QualityDecreaseAtHigherRateList.indexOf(item.name) !== -1) {
      amountToDecrease = amountToDecrease * 2
    }
    item.quality -= amountToDecrease
    if (item.quality < 0) {
      item.quality = 0
    }
  }

  private increaseQuality(item: Item): void {
    item.quality++
    if (QualityIncreaseAtHigherRateList.indexOf(item.name) !== -1 && item.sellIn <= 10) {
      item.quality++
      if (item.sellIn <= 5) {
        item.quality++
      }
    }

    if (item.sellIn < 0 && QualityBecomesZeroAfterSellIn.indexOf(item.name) !== -1) {
      item.quality = 0
    }

    if (item.quality > 50) {
      item.quality = 50
    }
  }

  private shouldQualityIncrease(item: Item): boolean {
    if (QualityIncreaseList.indexOf(item.name) !== -1) {
      return true
    }
    if (QualityIncreaseAfterSellInList.indexOf(item.name) !== -1 && item.sellIn < 0 ) {
      return true
    }
    return false
  }

  private shouldQualityChange(item: Item): boolean {
    if (LegendaryItemList.indexOf(item.name) !== -1) {
      return false
    }
    return true
  }
}
