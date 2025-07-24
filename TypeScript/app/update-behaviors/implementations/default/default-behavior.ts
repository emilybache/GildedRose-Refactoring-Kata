import { Item } from "@app/item";
import { IUpdateBehavior } from "../../update-behavior.interface";

export class DefaultBehavior implements IUpdateBehavior {
  constructor(private item: Item) {}

  update(): Item {
    const amountToSubtract = this.item.sellIn <= 0 ? 2 : 1;
    this.item.quality = Math.max(this.item.quality - amountToSubtract, 0);

    this.item.sellIn -= 1;

    return this.item;
  }
}
