import { Item } from "@app/item";
import { getUpdateBehaviorFor } from "./behavior-resolver";
import { AgedBrieBehavior } from "./implementations/aged-brie-behavior";
import { LegacyBehavior } from "./implementations/legacy-behavior";

describe("Behavior resolver", () => {
  it("should correctly resolve Aged Brie", () => {
    expect(getUpdateBehaviorFor(new Item("Aged Brie", 0, 0))).toBeInstanceOf(
      AgedBrieBehavior
    );
  });

  it("should correctly resolve the rest to Legacy behavior", () => {
    expect(
      getUpdateBehaviorFor(new Item("some other item", 0, 0))
    ).toBeInstanceOf(LegacyBehavior);
  });
});
