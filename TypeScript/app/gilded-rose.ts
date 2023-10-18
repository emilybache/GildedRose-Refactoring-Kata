import { Item } from "./itemClasses";

interface ItemForSale extends Item {
  handleQuality: () => void;
  handleSellIn: () => void;
}

export class GildedRose {
  items: ItemForSale[];
  constructor(items = [] as ItemForSale[]) {
    this.items = items;
  }

  updateQuality() {
    return this.items.map((item) => {
      item.handleSellIn();
      item.handleQuality();

      return item;
    });
  }
}
