import { Item } from "@app/item";
import { getUpdateBehaviorFor } from "@app/update-behaviors";

export class GildedRose {
  // also can't edit this because of the kata rules.
  // But I prefer typing this as : Item[]
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  updateQuality() {
    return this.items.map((item) => {
      return getUpdateBehaviorFor(item).update();
    });
  }
}
