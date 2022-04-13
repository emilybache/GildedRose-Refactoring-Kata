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
    this.items.forEach((item, i) => {
      item.quality = updateItemQuality(item)

      if (item.name != 'Sulfuras, Hand of Ragnaros') {
        item.sellIn = item.sellIn - 1;
      }

      item.quality = sellInBelow0(item)
    })

    return this.items;
  }
}


function updateItemQuality(item: Item): number {
  if (item.name === 'Sulfuras, Hand of Ragnaros') {
    return item.quality
  }
  
  if (item.name == 'Aged Brie') {
    return incrementQuality(item);
  }

  if (item.name == 'Backstage passes to a TAFKAL80ETC concert') {
    let itemCopy = { ...item }
    itemCopy.quality = incrementQuality(itemCopy);
    if (item.sellIn < 11) itemCopy.quality = incrementQuality(itemCopy)
    if (item.sellIn < 6) itemCopy.quality = incrementQuality(itemCopy)
    return itemCopy.quality
  }

  if (item.quality > 0) {
    return item.quality - 1
  }

  return item.quality
}


function incrementQuality(item: Item): number {
  if (item.quality < 50) {
    return item.quality + 1
  }
  return item.quality
}

function sellInBelow0({quality, name, sellIn}: Item): number {
  if (sellIn >= 0) return quality

  if (name == 'Sulfuras, Hand of Ragnaros') return quality;

  if (name == 'Aged Brie') {
    if (quality < 50) {
      return quality + 1
    }
    return quality
  }

  if (name == 'Backstage passes to a TAFKAL80ETC concert') {
    return 0
  } 

  if (quality > 0) {
    return quality - 1
  }

  return quality
}
