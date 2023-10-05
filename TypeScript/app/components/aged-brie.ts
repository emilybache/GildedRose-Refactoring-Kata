import { Item } from "./Item";

export class AgedBrie extends Item {
    updateQuality() {
      super.updateQuality();
      this.quality = Math.min(this.quality + 1, 50);
    }
  }