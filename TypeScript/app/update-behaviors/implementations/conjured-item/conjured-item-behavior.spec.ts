import { Item } from "@app/item";
import { ConjuredItemBehavior } from "./conjured-item-behavior";

describe("Conjured Item Behavior", () => {
  it("should degrade quality with 2 and sellIn with 1 when sellIn is over 0", () => {
    const behavior = new ConjuredItemBehavior(
      new Item("Conjured Mana Cake", 1, 4)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Conjured Mana Cake",
      sellIn: 0,
      quality: 2,
    });
  });
  it("should degrade quality with 4 and sellin with 1 when sellIn is equal to zero", () => {
    const behavior = new ConjuredItemBehavior(
      new Item("Conjured Mana Cake", 0, 5)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Conjured Mana Cake",
      sellIn: -1,
      quality: 1,
    });
  });

  it("should degrade quality with 4 and sellin with 1 when sellIn is under zero", () => {
    const behavior = new ConjuredItemBehavior(
      new Item("Conjured Mana Cake", -1, 5)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Conjured Mana Cake",
      sellIn: -2,
      quality: 1,
    });
  });

  it("shouldn't degrade quality under 0", () => {
    const behavior = new ConjuredItemBehavior(
      new Item("Conjured Mana Cake", 1, 1)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Conjured Mana Cake",
      sellIn: 0,
      quality: 0,
    });
  });
});
