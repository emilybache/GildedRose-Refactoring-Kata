import { Item } from "@app/item";
import { BackstagePassBehavior } from "./backstage-pass-behavior";

describe("Backstage Pass Behavior", () => {
  it("should increase quality of Backstage passes by 1 if sell in date is more than 10 days away", () => {
    const behavior = new BackstagePassBehavior(
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
    const behavior = new BackstagePassBehavior(
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
    const behavior = new BackstagePassBehavior(
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
    const behavior = new BackstagePassBehavior(
      new Item("Backstage passes to a TAFKAL80ETC concert", 0, 20)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Backstage passes to a TAFKAL80ETC concert",
      sellIn: -1,
      quality: 0,
    });
  });

  it("should not increase over 50", () => {
    const behavior = new BackstagePassBehavior(
      new Item("Backstage passes to a TAFKAL80ETC concert", 4, 50)
    );

    const result = behavior.update();

    expect(result).toMatchObject({
      name: "Backstage passes to a TAFKAL80ETC concert",
      sellIn: 3,
      quality: 50,
    });
  });
});
