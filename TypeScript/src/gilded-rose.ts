export class Item {
  name: string
  sellIn: number
  quality: number

  constructor(name, sellIn, quality) {
    this.name = name
    this.sellIn = sellIn
    this.quality = quality
  }

  static normalMaxQuality = 50
  static legendaryQuality = 80
}

export class GildedRose {
  items: Array<Item>

  constructor(items = [] as Array<Item>) {
    this.items = items
  }

  updateQuality() {
    this.items.forEach(item => {
      const currentProductName = item.name
      // PART 1
      if (
        currentProductName != 'Aged Brie' &&
        currentProductName != 'Backstage passes to a TAFKAL80ETC concert'
      ) {
        if (item.quality > 0) {
          if (currentProductName != 'Sulfuras, Hand of Ragnaros') {
            item.quality = item.quality - 1
          }
        }
      } else {
        if (item.quality < 50) {
          item.quality = item.quality + 1
          if (currentProductName == 'Backstage passes to a TAFKAL80ETC concert') {
            if (item.sellIn < 11) {
              if (item.quality < 50) {
                item.quality = item.quality + 1
              }
            }
            if (item.sellIn < 6) {
              if (item.quality < 50) {
                item.quality = item.quality + 1
              }
            }
          }
        }
      }
      // part 2
      if (currentProductName != 'Sulfuras, Hand of Ragnaros') {
        item.sellIn = item.sellIn - 1
      }
      // part 3
      if (item.sellIn < 0) {
        if (currentProductName != 'Aged Brie') {
          if (currentProductName != 'Backstage passes to a TAFKAL80ETC concert') {
            if (item.quality > 0) {
              if (currentProductName != 'Sulfuras, Hand of Ragnaros') {
                item.quality = item.quality - 1
              }
            }
          } else {
            item.quality = item.quality - item.quality
          }
        } else {
          if (item.quality < 50) {
            item.quality = item.quality + 1
          }
        }
      }
    })

    return this.items
  }
}
