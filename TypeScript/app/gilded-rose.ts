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

  private static updateSulfuras(item: Item) {
    // do nothing
  }

  private static updateBackstagePass(item: Item) {
    if (10 <= item.sellIn)
      item.quality += 1;
    else if (5 <= item.sellIn && item.sellIn < 10)
      item.quality += 2;
    else if (0 <= item.sellIn && item.sellIn < 5)
      item.quality += 3;
    else
      item.quality = 0;
  }

  private static updateAgedBrie(item: Item) {
    ++item.quality;
    if (item.sellIn < 0)
      ++item.quality;
  }

  private static updateRegularItem(item: Item) {
    --item.quality;
    if (item.sellIn < 0)
      --item.quality;
  }

  private static reboundQuality(item: Item) {
    if (item.quality < 0)
      item.quality = 0;
    if (item.quality > 50)
      item.quality = 50;
  }

  private updateItemQuality(item: Item) {
    --item.sellIn;
    switch (item.name) {
      case 'Sulfuras, Hand of Ragnaros':
        GildedRose.updateSulfuras(item);
        break;
      case 'Backstage passes to a TAFKAL80ETC concert':
        GildedRose.updateBackstagePass(item);
        break;
      case 'Aged Brie':
        GildedRose.updateAgedBrie(item);
        break;
      default:
        GildedRose.updateRegularItem(item);
    }
    GildedRose.reboundQuality(item);
  }

  updateQuality() {
    for (let i = 0; i < this.items.length; i++)
      this.updateItemQuality(this.items[i]);
    return this.items;
  }
}
