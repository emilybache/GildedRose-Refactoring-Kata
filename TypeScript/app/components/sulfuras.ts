import { ItemConstants } from "@/constants/constant";
import { Item } from "./Item";

export class Sulfuras extends Item {
  constructor() {
    super(ItemConstants.SULFURAS, 0, 80);
  }

  updateQuality() {
    // Sulfuras quality never changes
  }
}