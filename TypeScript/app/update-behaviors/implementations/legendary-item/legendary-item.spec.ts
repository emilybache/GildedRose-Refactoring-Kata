import { Item } from "@app/item";
import { LegendaryItemBehavior } from "./legendary-item-behavior";

describe("Legendary Item Behavior", () => {
  it("should keep items of quality 80 the same", () => {
    const behavior = new LegendaryItemBehavior(
      new Item("Sulfuras, Hand of Ragnaros", 0, 80)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Sulfuras, Hand of Ragnaros",
      sellIn: 0,
      quality: 80,
    });
  });

  it("should throw an error when a legendary item doesn't have a quality of 80", () => {
    const behavior = new LegendaryItemBehavior(
      new Item("Sulfuras, Hand of Ragnaros", 0, 5)
    );

    expect(() => behavior.update()).toThrow(
      "A Legendary Item cannot have a quality other than 80"
    );
  });
});
