import { Item } from "@app/item";
import { IUpdateBehavior } from "./update-behavior.interface";
import { LegacyBehavior } from "./implementations/legacy-behavior";
import { AgedBrieBehavior } from "./implementations/aged-brie/aged-brie-behavior";
import { BackstagePassBehavior } from "./implementations/backstage-pass/backstage-pass-behavior";

export function getUpdateBehaviorFor(item: Item): IUpdateBehavior {
  switch (item.name) {
    case "Aged Brie":
      return new AgedBrieBehavior(item);
    case "Backstage passes to a TAFKAL80ETC concert":
      return new BackstagePassBehavior(item);
    default:
      return new LegacyBehavior(item);
  }
}
