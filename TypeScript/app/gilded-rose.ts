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
    .map((item) => ({ ...item, quality: updateItemQuality(item) }))
    .map((item) => ({ ...item, sellIn: updateItemSellIn(item) }))
    .map(item => ({ ...item, quality: sellInBelow0(item) }))
}

function updateItemSellIn({name, sellIn}: Item) {
  if (name == 'Sulfuras, Hand of Ragnaros') return sellIn

  return sellIn - 1;
}

function updateItemQuality({name, quality, sellIn, ...rest}: Item): number {
  if (name === 'Sulfuras, Hand of Ragnaros') return quality
  
  if (name == 'Aged Brie') return incrementQuality({quality});

  if (name == 'Backstage passes to a TAFKAL80ETC concert') {
    let itemCopy = { name, quality, sellIn, ...rest }
    itemCopy.quality = incrementQuality(itemCopy);
    if (sellIn < 11) itemCopy.quality = incrementQuality(itemCopy)
    if (sellIn < 6) itemCopy.quality = incrementQuality(itemCopy)
    return itemCopy.quality
  }

  return decrementQuality({quality})
}

function sellInBelow0({ quality, name, sellIn }: Item): number {
  if (sellIn >= 0) return quality

  if (name == 'Sulfuras, Hand of Ragnaros') return quality

  if (name == 'Aged Brie') return incrementQuality({quality})

  if (name == 'Backstage passes to a TAFKAL80ETC concert') return 0
   
  return decrementQuality({quality})
}

function decrementQuality({ quality }: Pick<Item, 'quality'>): number {
  if (quality < 1) return quality
  return quality - 1
}

function incrementQuality({ quality }: Pick<Item, 'quality'>): number {
  if (quality < 50) return quality + 1
  return quality
}