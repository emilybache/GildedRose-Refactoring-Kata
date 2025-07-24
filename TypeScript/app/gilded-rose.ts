export class Item {
  name: string;
  sellIn: number;
  quality: number;

  // can't edit this constructor because of the kata rules.
  // but I would change the constructor to take a javaScript object.
  // It makes the consumer code more readable.
  // e.g. new Item({ name: "standard item", sellIn: 0, quality: 2 })
  // instead of new Item("standard item", 0, 2)

  // I would also type the parameters, because now they are implicitly any.
  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

export class GildedRose {
  // also can't edit this because of the kata rules.
  // But I prefer typing this as : Item[]
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  updateQuality() {
    return this.items.map((item) => {
      if (
        item.name !== "Aged Brie" &&
        item.name !== "Backstage passes to a TAFKAL80ETC concert"
      ) {
        if (item.quality > 0) {
          if (item.name !== "Sulfuras, Hand of Ragnaros") {
            item.quality = item.quality - 1;
          }
        }
      } else {
        if (item.quality < 50) {
          item.quality = item.quality + 1;
          if (item.name === "Backstage passes to a TAFKAL80ETC concert") {
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
      if (item.name !== "Sulfuras, Hand of Ragnaros") {
        item.sellIn = item.sellIn - 1;
      }
      if (item.sellIn < 0) {
        if (item.name !== "Aged Brie") {
          if (item.name !== "Backstage passes to a TAFKAL80ETC concert") {
            if (item.quality > 0) {
              if (item.name !== "Sulfuras, Hand of Ragnaros") {
                item.quality = item.quality - 1;
              }
            }
          } else {
            item.quality = item.quality - item.quality;
          }
        } else {
          if (item.quality < 50) {
            item.quality = item.quality + 1;
          }
        }
      }
      return item;
    });
  }
}
