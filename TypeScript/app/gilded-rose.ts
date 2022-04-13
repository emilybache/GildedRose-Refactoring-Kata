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
      item.quality = this.updateItemQuality(item)

      if (item.name != 'Sulfuras, Hand of Ragnaros') {
        item.sellIn = item.sellIn - 1;
      }

      item.quality = sellInBelow0(item)
    })

    return this.items;
  }

  private updateItemQuality(item: Item): number {
    if (item.name === 'Sulfuras, Hand of Ragnaros') return item.quality
    
    if (item.name == 'Aged Brie') {
      this.incrementQuality(item);
      return item.quality
    }

    if (item.name == 'Backstage passes to a TAFKAL80ETC concert') {
      this.incrementQuality(item);
      if (item.sellIn < 11) this.incrementQuality(item)
      if (item.sellIn < 6) this.incrementQuality(item)
      return item.quality
    }

    if (item.quality > 0) {
      item.quality = item.quality - 1
      return item.quality
    }

    return item.quality
  }
  

  private incrementQuality(item: Item) {
    if (item.quality < 50) {
      item.quality = item.quality + 1;
    }
  }
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
