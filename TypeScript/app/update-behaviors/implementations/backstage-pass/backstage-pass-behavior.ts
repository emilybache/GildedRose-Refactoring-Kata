import { config } from "@app/config";
import { Item } from "@app/item";
import { IUpdateBehavior } from "@app/update-behaviors";

export class BackstagePassBehavior implements IUpdateBehavior {
  constructor(private item: Item) {}
  update(): Item {
    const sellIn = this.item.sellIn;
    const amountToIncrease = this.#getAmountToIncrease(sellIn);

    this.item.quality = Math.min(
      this.item.quality + amountToIncrease,
      config.maxQuality
    );

    if (sellIn <= 0) {
      this.item.quality = 0;
    }

    this.item.sellIn -= 1;

    return this.item;
  }

  #getAmountToIncrease(sellIn: number): number {
    if (sellIn <= 5) return 3;
    if (sellIn <= 10) return 2;
    return 1;
  }
}
