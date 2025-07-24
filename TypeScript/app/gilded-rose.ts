import { Item } from "@app/item";
import { IUpdateBehavior, LegacyBehavior } from "@app/update-behaviors";

export class GildedRose {
  // also can't edit this because of the kata rules.
  // But I prefer typing this as : Item[]
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  updateQuality() {
    return this.items.map((item) => {
      return this.#getUpdateBehaviorFor(item).update();
    });
  }

  #getUpdateBehaviorFor(item: Item): IUpdateBehavior {
    switch (item.name) {
      default:
        return new LegacyBehavior(item);
    }
  }
}
