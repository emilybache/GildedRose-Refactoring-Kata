import { Item, GildedRose } from "@/gilded-rose";

describe("Gilded Rose", () => {
  let gildedRose: GildedRose;

  beforeEach(() => {
    gildedRose = new GildedRose([
      new Item("foo", 0, 0),
      new Item("Conjured item", 10, 8),
      new Item("Sulfuras item", 0, 80),
      new Item("Aged Brie item ", 10, 33),
      new Item("Aged Brie item", 0, 50),
      new Item("Backstage passes item", 8, 30),
    ]);
  });

  it("Should foo", () => {
    const items = gildedRose.updateQuality();
    expect(items[0].name).toBe("foo");
  });

  it("The Quality of Backstage passes item increases by 2 when there are 10 days or less", () => {
    const items = gildedRose.updateQuality();
    expect(items[5].quality).toBe(32);
  });

  it("The quality of Conjured item should be degraded by twice more than normal items", () => {
    const items = gildedRose.updateQuality();
    expect(items[1].quality).toBe(6);
  });

  it("Aged Brie item actually increases in Quality the older it gets", () => {
    const items = gildedRose.updateQuality();
    expect(items[3].quality).toBe(34);
  });

  it("The quality of Sulfuras item should remain unchanged", () => {
    const items = gildedRose.updateQuality();
    expect(items[2].quality).toBe(80);
  });

  it("The Quality of an item is never more than 50", () => {
    const items = gildedRose.updateQuality();
    expect(items[4].quality).toBe(50);
  });
});
