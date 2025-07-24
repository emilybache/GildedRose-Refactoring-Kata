import { Item } from "@app/item";
import { LegacyBehavior } from "./legacy-behavior";

describe("Legacy Behavior", () => {
  it("should degrade sell inn and quality each day", () => {
    const behavior = new LegacyBehavior(new Item("standard item", 1, 1));

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "standard item",
      sellIn: 0,
      quality: 0,
    });
  });

  it("should degrade quality twice as fast after sell in date", () => {
    const behavior = new LegacyBehavior(new Item("standard item", 0, 2));

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "standard item",
      sellIn: -1,
      quality: 0,
    });
  });

  it("should not degrade quality below 0", () => {
    const behavior = new LegacyBehavior(new Item("standard item", 1, 0));

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "standard item",
      sellIn: 0,
      quality: 0,
    });
  });
});
