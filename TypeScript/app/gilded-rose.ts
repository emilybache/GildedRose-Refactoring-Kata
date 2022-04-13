export class Item {
  name: string;
  sellIn: number;
  quality: number;

  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

export class GildedRose {
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  updateQuality() {
    this.items = updateItems(this.items)
    return this.items;
  }
}


function updateItems(items: Item[]): Item[] {
  return items 
    .map(item => ({ ...item, quality: updateItemQuality(item) }))
    .map(item => ({ ...item, sellIn: updateItemSellIn(item) }))
    .map(item => ({ ...item, quality: updateExpiredItemQuality(item) }))
}

function updateItemQuality({ name, quality, sellIn, ...rest }: Item): number {
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

function updateItemSellIn({ name, sellIn }: Item) {
  switch (name) {
    case 'Sulfuras, Hand of Ragnaros': return sellIn
    default: return sellIn - 1
  }
}

function updateExpiredItemQuality({ quality, name, sellIn }: Item): number {
  if (sellIn >= 0) return quality

  switch (name) {
    case 'Sulfuras, Hand of Ragnaros': return quality
    case 'Aged Brie': return incrementQuality({ quality })
    case 'Backstage passes to a TAFKAL80ETC concert': return 0
    default: return decrementQuality({ quality })
  }
}

function decrementQuality({ quality }: Pick<Item, 'quality'>): number {
  if (quality < 1) return quality
  return quality - 1
}

function incrementQuality({ quality }: Pick<Item, 'quality'>): number {
  if (quality < 50) return quality + 1
  return quality
}