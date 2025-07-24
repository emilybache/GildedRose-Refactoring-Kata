import { config } from "@app/config";
import { Item } from "@app/item";
import { IUpdateBehavior } from "@app/update-behaviors";

export class AgedBrieBehavior implements IUpdateBehavior {
  constructor(private item: Item) {}

  update(): Item {
    const isPastSellInDay = this.item.sellIn <= 0;

    const amountToAdd = isPastSellInDay ? 2 : 1;

    this.item.quality = Math.min(
      config.maxQuality,
      this.item.quality + amountToAdd
    );

    this.item.sellIn -= 1;

    return this.item;
  }
}
