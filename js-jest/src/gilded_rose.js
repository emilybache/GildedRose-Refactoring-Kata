class Item {
  constructor(name, sellIn, quality) {
    this.name = name
    this.sellIn = sellIn
    this.quality = quality
  }
}

const calculateQualityDiffForNormalItem = ({ sellIn, quality }) => {
  const isQualityGreaterThanZero = quality > 0
  const cannotSell = sellIn < 0

  if (isQualityGreaterThanZero && cannotSell) return -2
  if (isQualityGreaterThanZero) return -1
  return 0
}

const calculateQualityDiffForBackstage = ({ sellIn, quality }) => {
  const tenDaysOrLessToSell = sellIn <= 10
  const fiveDaysOrLessToSell = sellIn <= 5
  const cannotSell = sellIn < 0

  if (cannotSell) return -quality
  if (fiveDaysOrLessToSell) return +3
  if (tenDaysOrLessToSell) return +2

  return +1
}

const calculateSellinDifference = ({ name }) => {
  const isSulfuras = name == 'Sulfuras'
  return !isSulfuras ? -1 : 0
}

const calculateQualityDifference = (item) => {
  const isSulfuras = item.name == 'Sulfuras'
  const isAgedBrie = item.name == 'Aged Brie'
  const isConjuredItem = item.name.includes('Conjured')
  const isBackstagePasses = item.name == 'Backstage passes'
  const isQualityLessThan50 = item.quality < 50
  const isNormalItem =
    !isAgedBrie && !isBackstagePasses && !isSulfuras && !isConjuredItem

  if (isConjuredItem) return calculateQualityDiffForNormalItem(item) * 2
  if (isNormalItem) return calculateQualityDiffForNormalItem(item)
  if (isBackstagePasses) return calculateQualityDiffForBackstage(item)
  if (isAgedBrie && isQualityLessThan50) return +1
  return 0
}

class Shop {
  constructor(items = []) {
    this.items = items
  }

  updateQuality() {
    return this.items.map((item) => {
      item.sellIn += calculateSellinDifference(item)
      item.quality += calculateQualityDifference(item)
      return item
    })
  }
}

module.exports = {
  Item,
  Shop,
}
