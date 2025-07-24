import { Item } from "@app/item";
import { IUpdateBehavior } from "@app/update-behaviors/update-behavior.interface";

export class ConjuredItemBehavior implements IUpdateBehavior {
  constructor(private item: Item) {}

  update(): Item {
    const amountToSubtract = this.item.sellIn <= 0 ? 4 : 2;

    this.item.quality = Math.max(this.item.quality - amountToSubtract, 0);

    this.item.sellIn -= 1;

    return this.item;
  }
}
