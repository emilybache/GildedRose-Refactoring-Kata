import { GildedRose } from "@app/gilded-rose";
import { Item } from "@app/item";

describe("Gilded Rose", () => {
  it("should have an empty array as items when no constructor parameter is provided", () => {
    const gildedRose = new GildedRose();
    expect(gildedRose.items).toEqual([]);
  });

  it("should degrade sell inn and quality each day", () => {
    const gildedRose = new GildedRose([new Item("standard item", 1, 1)]);

    const items = gildedRose.updateQuality();

    expect(items[0]).toMatchObject({
      name: "standard item",
      sellIn: 0,
      quality: 0,
    });
  });

  it("should degrade quality twice as fast after sell in date", () => {
    const gildedRose = new GildedRose([new Item("standard item", 0, 2)]);

    const items = gildedRose.updateQuality();

    expect(items[0]).toMatchObject({
      name: "standard item",
      sellIn: -1,
      quality: 0,
    });
  });

  it("should not degrade quality below 0", () => {
    const gildedRose = new GildedRose([new Item("standard item", 1, 0)]);

    const items = gildedRose.updateQuality();

    expect(items[0]).toMatchObject({
      name: "standard item",
      sellIn: 0,
      quality: 0,
    });
  });

  it("should increase quality of Aged Brie as it gets older", () => {
    const gildedRose = new GildedRose([new Item("Aged Brie", 1, 1)]);

    const items = gildedRose.updateQuality();

    expect(items[0]).toMatchObject({
      name: "Aged Brie",
      sellIn: 0,
      quality: 2,
    });
  });

  it("should increase quality of Aged Brie twice as fast after sell in date", () => {
    const gildedRose = new GildedRose([new Item("Aged Brie", 0, 1)]);

    const items = gildedRose.updateQuality();

    expect(items[0]).toMatchObject({
      name: "Aged Brie",
      sellIn: -1,
      quality: 3,
    });
  });

  it("should never increase quality of Aged Brie over 50", () => {
    const gildedRose = new GildedRose([new Item("Aged Brie", 0, 50)]);

    const items = gildedRose.updateQuality();

    expect(items[0]).toMatchObject({
      name: "Aged Brie",
      sellIn: -1,
      quality: 50,
    });
  });

  it("should not change quality of Sulfuras", () => {
    const gildedRose = new GildedRose([
      new Item("Sulfuras, Hand of Ragnaros", 0, 80),
    ]);

    const items = gildedRose.updateQuality();

    expect(items[0]).toMatchObject({
      name: "Sulfuras, Hand of Ragnaros",
      sellIn: 0,
      quality: 80,
    });
  });

  it("should increase quality of Backstage passes by 1 if sell in date is more than 10 days away", () => {
    const gildedRose = new GildedRose([
      new Item("Backstage passes to a TAFKAL80ETC concert", 11, 20),
    ]);

    const items = gildedRose.updateQuality();

    expect(items[0]).toMatchObject({
      name: "Backstage passes to a TAFKAL80ETC concert",
      sellIn: 10,
      quality: 21,
    });
  });

  it("should increase quality of Backstage passes by 2 if sell in date is less than 10 days away but more than 5", () => {
    const gildedRose = new GildedRose([
      new Item("Backstage passes to a TAFKAL80ETC concert", 9, 20),
    ]);

    const items = gildedRose.updateQuality();

    expect(items[0]).toMatchObject({
      name: "Backstage passes to a TAFKAL80ETC concert",
      sellIn: 8,
      quality: 22,
    });
  });

  it("should increase quality of Backstage passes by 3 if sell in date is less than 5 days away", () => {
    const gildedRose = new GildedRose([
      new Item("Backstage passes to a TAFKAL80ETC concert", 4, 20),
    ]);

    const items = gildedRose.updateQuality();

    expect(items[0]).toMatchObject({
      name: "Backstage passes to a TAFKAL80ETC concert",
      sellIn: 3,
      quality: 23,
    });
  });

  it("should drop quality of Backstage passes to 0 after sell in date", () => {
    const gildedRose = new GildedRose([
      new Item("Backstage passes to a TAFKAL80ETC concert", 0, 20),
    ]);

    const items = gildedRose.updateQuality();

    expect(items[0]).toMatchObject({
      name: "Backstage passes to a TAFKAL80ETC concert",
      sellIn: -1,
      quality: 0,
    });
  });

  // to implement: "Conjured" items degrade in Quality twice as fast as normal items
});
