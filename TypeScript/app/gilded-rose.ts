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
      if (item.name != 'Aged Brie' && item.name != 'Backstage passes to a TAFKAL80ETC concert') {
        if (item.quality > 0) {
          if (item.name != 'Sulfuras, Hand of Ragnaros') {
            item.quality = item.quality - 1
          }
        }
      } else {
        if (item.quality < 50) {
          item.quality = item.quality + 1
          if (item.name == 'Backstage passes to a TAFKAL80ETC concert') {
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
      if (item.name != 'Sulfuras, Hand of Ragnaros') {
        item.sellIn = item.sellIn - 1;
      }
      item.quality = this.sellInBelow0(item)
    })

    return this.items;
  }

  private sellInBelow0(item: Item): number {
    if (item.sellIn >= 0) return item.quality

    if (item.name == 'Sulfuras, Hand of Ragnaros') return item.quality;

    if (item.name == 'Aged Brie') {
      if (item.quality < 50) {
        return item.quality + 1
      }
      return item.quality
    }

    if (item.name == 'Backstage passes to a TAFKAL80ETC concert') {
      return 0
    } 

    if (item.quality > 0) {
      return item.quality - 1
    }

    return item.quality
  }
}
