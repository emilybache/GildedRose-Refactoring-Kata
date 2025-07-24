import { Item } from "@app/item";
import { AgedBrieBehavior } from "./aged-brie-behavior";

describe("AgedBrie Behavior", () => {
  it("should increase quality of Aged Brie as it gets older", () => {
    const behavior = new AgedBrieBehavior(new Item("Aged Brie", 1, 1));

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Aged Brie",
      sellIn: 0,
      quality: 2,
    });
  });

  it("should increase quality of Aged Brie twice as fast after sell in date", () => {
    const behavior = new AgedBrieBehavior(new Item("Aged Brie", 0, 1));

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Aged Brie",
      sellIn: -1,
      quality: 3,
    });
  });

  it("should never increase quality of Aged Brie over 50", () => {
    const behavior = new AgedBrieBehavior(new Item("Aged Brie", 0, 50));

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Aged Brie",
      sellIn: -1,
      quality: 50,
    });
  });
});
