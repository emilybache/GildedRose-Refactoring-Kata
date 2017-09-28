class Item {
  constructor(name, sellIn, quality) {
    this.name = name
    this.sellIn = sellIn
    this.quality = quality
  }
}

class Shop {
  constructor(items = []) {
    this.items = items
  }

  downgradeQuality(item, value = 1) {
    item.quality -= value
  }

  upgradeQuality(item, value = 1) {
    item.quality += value
  }

  downgradeSellIn(item, value = 1) {
    item.sellIn -= value
  }

  hasQuality(item) {
    return item.quality > 0
  }

  commonQuality(item) {
    return item.quality < 50
  }

  updateQuality() {
    this.items.map((item, i) => {

      if (
        $.inArray(item.name, [
          'Aged Brie',
          'Backstage passes to a TAFKAL80ETC concert',
          'Sulfuras, Hand of Ragnaros',
        ]) == -1 && this.hasQuality(item)
      ) {
        this.downgradeQuality(item)
      }

      else {
        if (this.commonQuality(item)) {
          this.upgradeQuality(item)

          if (item.name == 'Backstage passes to a TAFKAL80ETC concert') {
            if (item.sellIn < 11 && this.commonQuality(item))
              this.upgradeQuality(item)

            if (item.sellIn < 6 && this.commonQuality(item))
              this.upgradeQuality(item)
          }
        }
      }

      if (item.name != 'Sulfuras, Hand of Ragnaros') this.downgradeSellIn(item)

      if (item.sellIn < 0) {
        if (item.name != 'Aged Brie') {
          if (item.name != 'Backstage passes to a TAFKAL80ETC concert') {
            if (item.name != 'Sulfuras, Hand of Ragnaros' &&
                this.hasQuality(item))
              this.downgradeQuality(item)
          }
          else {
            this.downgradeQuality(item, item.quality)
          }
        }
        else if (this.commonQuality(item)) {
          this.upgradeQuality(item)
        }
      }

    })

    return this.items
  }
}
