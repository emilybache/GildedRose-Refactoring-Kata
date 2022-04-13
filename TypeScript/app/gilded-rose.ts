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


function updateItemQuality({name, quality, sellIn, ...rest}: Item): number {
  if (name === 'Sulfuras, Hand of Ragnaros') {
    return quality
  }
  
  if (name == 'Aged Brie') {
    return incrementQuality({quality});
  }

  if (name == 'Backstage passes to a TAFKAL80ETC concert') {
    let itemCopy = { name, quality, sellIn, ...rest }
    itemCopy.quality = incrementQuality(itemCopy);
    if (sellIn < 11) itemCopy.quality = incrementQuality(itemCopy)
    if (sellIn < 6) itemCopy.quality = incrementQuality(itemCopy)
    return itemCopy.quality
  }

  return quality > 0 ? quality - 1 : quality
}

function sellInBelow0({ quality, name, sellIn }: Item): number {
  if (sellIn >= 0) return quality

  if (name == 'Sulfuras, Hand of Ragnaros') return quality

  if (name == 'Aged Brie') return incrementQuality({quality})

  if (name == 'Backstage passes to a TAFKAL80ETC concert') return 0
   
  return quality > 0 ? quality - 1 : quality
}

function incrementQuality({ quality }: Pick<Item, 'quality'>): number {
  if (quality < 50) return quality + 1
  return quality
}