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

  it("should not change quality of Sulfuras", () => {
    const behavior = new LegacyBehavior(
      new Item("Sulfuras, Hand of Ragnaros", 0, 80)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Sulfuras, Hand of Ragnaros",
      sellIn: 0,
      quality: 80,
    });
  });

  it("should increase quality of Backstage passes by 1 if sell in date is more than 10 days away", () => {
    const behavior = new LegacyBehavior(
      new Item("Backstage passes to a TAFKAL80ETC concert", 11, 20)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Backstage passes to a TAFKAL80ETC concert",
      sellIn: 10,
      quality: 21,
    });
  });

  it("should increase quality of Backstage passes by 2 if sell in date is less than 10 days away but more than 5", () => {
    const behavior = new LegacyBehavior(
      new Item("Backstage passes to a TAFKAL80ETC concert", 9, 20)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Backstage passes to a TAFKAL80ETC concert",
      sellIn: 8,
      quality: 22,
    });
  });

  it("should increase quality of Backstage passes by 3 if sell in date is less than 5 days away", () => {
    const behavior = new LegacyBehavior(
      new Item("Backstage passes to a TAFKAL80ETC concert", 4, 20)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Backstage passes to a TAFKAL80ETC concert",
      sellIn: 3,
      quality: 23,
    });
  });

  it("should drop quality of Backstage passes to 0 after sell in date", () => {
    const behavior = new LegacyBehavior(
      new Item("Backstage passes to a TAFKAL80ETC concert", 0, 20)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Backstage passes to a TAFKAL80ETC concert",
      sellIn: -1,
      quality: 0,
    });
  });
});
