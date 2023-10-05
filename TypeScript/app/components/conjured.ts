import { Item } from "./Item";

export class Conjured extends Item {
  updateQuality() {
    super.updateQuality();
    this.quality = Math.max(this.quality - 2, 0);
  }
}