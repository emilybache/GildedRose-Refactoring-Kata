import { Item } from "@app/item";
import { IUpdateBehavior } from "../update-behavior.interface";

export class LegacyBehavior implements IUpdateBehavior {
  constructor(private item: Item) {}

  update(): Item {
    if (this.item.quality > 0) {
      if (this.item.name !== "Sulfuras, Hand of Ragnaros") {
        this.item.quality = this.item.quality - 1;
      }
    }

    if (this.item.name !== "Sulfuras, Hand of Ragnaros") {
      this.item.sellIn = this.item.sellIn - 1;
    }
    if (this.item.sellIn < 0) {
      if (this.item.quality > 0) {
        if (this.item.name !== "Sulfuras, Hand of Ragnaros") {
          this.item.quality = this.item.quality - 1;
        }
      }
    }
    return this.item;
  }
}
