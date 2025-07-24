import { Item } from "@app/item";
import { IUpdateBehavior } from "./update-behavior.interface";

export class LegacyBehavior implements IUpdateBehavior {
  constructor(private item: Item) {}

  update(): Item {
    if (
      this.item.name !== "Aged Brie" &&
      this.item.name !== "Backstage passes to a TAFKAL80ETC concert"
    ) {
      if (this.item.quality > 0) {
        if (this.item.name !== "Sulfuras, Hand of Ragnaros") {
          this.item.quality = this.item.quality - 1;
        }
      }
    } else {
      if (this.item.quality < 50) {
        this.item.quality = this.item.quality + 1;
        if (this.item.name === "Backstage passes to a TAFKAL80ETC concert") {
          if (this.item.sellIn < 11) {
            if (this.item.quality < 50) {
              this.item.quality = this.item.quality + 1;
            }
          }
          if (this.item.sellIn < 6) {
            if (this.item.quality < 50) {
              this.item.quality = this.item.quality + 1;
            }
          }
        }
      }
    }
    if (this.item.name !== "Sulfuras, Hand of Ragnaros") {
      this.item.sellIn = this.item.sellIn - 1;
    }
    if (this.item.sellIn < 0) {
      if (this.item.name !== "Aged Brie") {
        if (this.item.name !== "Backstage passes to a TAFKAL80ETC concert") {
          if (this.item.quality > 0) {
            if (this.item.name !== "Sulfuras, Hand of Ragnaros") {
              this.item.quality = this.item.quality - 1;
            }
          }
        } else {
          this.item.quality = this.item.quality - this.item.quality;
        }
      } else {
        if (this.item.quality < 50) {
          this.item.quality = this.item.quality + 1;
        }
      }
    }
    return this.item;
  }
}
