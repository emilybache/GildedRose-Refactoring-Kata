import { Item } from "@app/item";
import { IUpdateBehavior } from "./update-behavior.interface";
import { LegacyBehavior } from "./implementations/legacy-behavior";
import { AgedBrieBehavior } from "./implementations/aged-brie-behavior";

export function getUpdateBehaviorFor(item: Item): IUpdateBehavior {
  switch (item.name) {
    case "Aged Brie":
      return new AgedBrieBehavior(item);
    default:
      return new LegacyBehavior(item);
  }
}
