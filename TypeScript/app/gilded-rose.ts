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

export type ConjuredItem = Item & { conjured: true }
export class GildedRose {
  items: Array<Item | ConjuredItem>

  constructor(items = [] as Array<Item | ConjuredItem>) {
    this.items = items
  }

  updateQuality() {
    this.items = [...updateItems(this.items)]
    return this.items
  }
}

function updateItems(items: readonly Readonly<Item | ConjuredItem>[]): readonly Readonly<Item | ConjuredItem>[] {
  return items 
    .map(item => ({ ...item, quality: updateItemQuality(item) }))
    .map(item => ({ ...item, sellIn: updateItemSellIn(item) }))
    .map(item => ({ ...item, quality: updateExpiredItemQuality(item) }))
    .map((item, i) => {
      const previousItem = items[i]
      return ({ ...item, quality: updateConjuredItemQuality(previousItem, item) })
    })
}

function updateItemQuality({ name, quality, sellIn, ...rest }: Readonly<Item>): number {
  switch (name) {
    case 'Sulfuras, Hand of Ragnaros': return quality
    case 'Aged Brie': return incrementQuality({ quality });
    case 'Backstage passes to a TAFKAL80ETC concert': {
      let itemCopy = { name, quality, sellIn, ...rest }
      itemCopy.quality = incrementQuality(itemCopy);
      if (sellIn < 11) itemCopy.quality = incrementQuality(itemCopy)
      if (sellIn < 6) itemCopy.quality = incrementQuality(itemCopy)
      return itemCopy.quality
    }
    default: return decrementQuality({ quality })
  }
}

function updateItemSellIn({ name, sellIn }: Readonly<Item>) {
  switch (name) {
    case 'Sulfuras, Hand of Ragnaros': return sellIn
    default: return sellIn - 1
  }
}

function updateExpiredItemQuality({ quality, name, sellIn }: Readonly<Item>): number {
  const isExpired = sellIn < 0
  if (!isExpired) return quality

  switch (name) {
    case 'Sulfuras, Hand of Ragnaros': return quality
    case 'Aged Brie': return incrementQuality({ quality })
    case 'Backstage passes to a TAFKAL80ETC concert': return 0
    default: return decrementQuality({ quality })
  }
}

function decrementQuality({ quality }: Readonly<Pick<Item, 'quality'>>): number {
  if (quality < 1) return quality
  return quality - 1
}

function incrementQuality({ quality }: Readonly<Pick<Item, 'quality'>>): number {
  if (quality >= 50) return quality
  return quality + 1
}

function updateConjuredItemQuality(previousItem: Item, currentItem: Item | ConjuredItem): number {
  const isConjured = 'conjured' in currentItem && currentItem.conjured
  if (!isConjured) return currentItem.quality
  const diff = previousItem.quality - currentItem.quality
  if (diff >= 0) return currentItem.quality

  return previousItem.quality - (diff * 2)
}