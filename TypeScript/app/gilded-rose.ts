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
      if (item.name == 'Aged Brie' || item.name == 'Backstage passes to a TAFKAL80ETC concert') {
        brieOrBackstageQuality(item);
      } else {
        if (item.quality > 0) {
          if (item.name != 'Sulfuras, Hand of Ragnaros') {
            item.quality = item.quality - 1
          }
        }
      }
      if (item.name != 'Sulfuras, Hand of Ragnaros') {
        item.sellIn = item.sellIn - 1;
      }
      item.quality = sellInBelow0(item)
    })

    return this.items;

    function brieOrBackstageQuality(item: Item) {
      if (item.quality < 50) {
        item.quality = item.quality + 1;
        if (item.name == 'Backstage passes to a TAFKAL80ETC concert') {
          if (item.sellIn < 11) {
            if (item.quality < 50) {
              item.quality = item.quality + 1;
            }
          }
          if (item.sellIn < 6) {
            if (item.quality < 50) {
              item.quality = item.quality + 1;
            }
          }
        }
      }
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
