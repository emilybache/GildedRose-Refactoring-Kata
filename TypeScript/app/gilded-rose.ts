import { AgedBrie, Passes, Surfras } from "./itemClasses";
type ItemClasses = AgedBrie[] | Surfras[] | Passes[];

export class GildedRose {
  items: ItemClasses;
  constructor(items = [] as ItemClasses) {
    this.items = items;
  }

  updateQuality() {
    const updatedItems = this.items.map((item) => {
      item.handleQuality();

      item.handleSellIn();
      return item;
    });
    return updatedItems as ItemClasses;
  }
}
