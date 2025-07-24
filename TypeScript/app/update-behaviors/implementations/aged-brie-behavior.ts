import { Item } from "@app/item";
import { IUpdateBehavior } from "../update-behavior.interface";

export class AgedBrieBehavior implements IUpdateBehavior {
  readonly #MAX_AMOUNT = 50;

  constructor(private item: Item) {}

  update(): Item {
    this.item.sellIn -= 1;

    const isPastSellInDay = this.item.sellIn < 0;

    const amountToAdd = isPastSellInDay ? 2 : 1;

    this.item.quality = Math.min(
      this.#MAX_AMOUNT,
      this.item.quality + amountToAdd
    );

    return this.item;
  }
}
