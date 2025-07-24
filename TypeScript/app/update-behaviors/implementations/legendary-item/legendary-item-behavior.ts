import { config } from "@app/config";
import { Item } from "@app/item";
import { IUpdateBehavior } from "@app/update-behaviors/update-behavior.interface";

export class LegendaryItemBehavior implements IUpdateBehavior {
  constructor(public item: Item) {}
  update(): Item {
    if (this.item.quality !== 80) {
      throw new Error("A Legendary Item cannot have a quality other than 80");
    }
    return this.item;
  }
}
