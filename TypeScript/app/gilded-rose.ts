enum ItemType {
  AGED_BRIE = "AGED_BRIE",
  BACKSTAGE_PASSES = "BACKSTAGE_PASSES",
  SULFURAS = "SULFURAS",
  NORMAL = "NORMAL",
}

export class Item {
  name: string;
  sellIn: number;
  quality: number;

  constructor(name: string, sellIn: number, quality: number) {
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

  private specifyItemType(item: Item): ItemType {
    if (item.name.includes("Aged Brie")) {
      return ItemType.AGED_BRIE;
    } else if (item.name.includes("Backstage passes")) {
      return ItemType.BACKSTAGE_PASSES;
    } else if (item.name.includes("Sulfuras")) {
      return ItemType.SULFURAS;
    } else {
      return ItemType.NORMAL;
    }
  }

  private updateItemQuality(item: Item, type: ItemType): void {
    switch (type) {
      case ItemType.AGED_BRIE:
        this.updateAgedBrieQuality(item);
        break;
      case ItemType.BACKSTAGE_PASSES:
        this.updateBackstagePassesQuality(item);
        break;
      case ItemType.SULFURAS:
        break;
      default:
        this.updateNormalItemQuality(item);
        break;
    }
  }

  private updateNormalItemQuality(item: Item): void {
    if (item.sellIn > 0) {
      this.decreaseQuality(item, 1);
    } else {
      this.decreaseQuality(item, 2);
    }
  }

  private updateAgedBrieQuality(item: Item): void {
    if (item.sellIn > 0) {
      this.increaseQuality(item, 1);
    } else {
      this.increaseQuality(item, 2);
    }
  }

  private updateBackstagePassesQuality(item: Item): void {
    if (item.sellIn <= 0) {
      item.quality = 0;
    } else if (item.sellIn <= 5) {
      this.increaseQuality(item, 3);
    } else if (item.sellIn <= 10) {
      this.increaseQuality(item, 2);
    } else {
      this.increaseQuality(item, 1);
    }
  }

  private updateItemSellIn(item: Item, type: ItemType): void {
    if (type !== ItemType.SULFURAS) {
      item.sellIn -= 1;
    }
  }
  private increaseQuality(item: Item, amount: number): void {
    item.quality = Math.min(item.quality + amount, 50);
  }
  private decreaseQuality(item: Item, amount: number): void {
    item.quality = Math.max(item.quality - amount, 0);
  }
  private updateSingleItem(item: Item): void {
    const type = this.specifyItemType(item);

    this.updateItemSellIn(item, type);
    this.updateItemQuality(item, type);
  }

  updateQuality() {
    for (const item of this.items) {
      this.updateSingleItem(item);
    }
    return this.items;
  }
}
