import { Item } from "@app/item";
import { IUpdateBehavior } from "./update-behavior.interface";
import { LegacyBehavior } from "./legacy-behavior";

export function getUpdateBehaviorFor(item: Item): IUpdateBehavior {
  switch (item.name) {
    default:
      return new LegacyBehavior(item);
  }
}
