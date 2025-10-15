import { Item, GildedRose } from "@/gilded-rose";

describe("GildedRose", () => {
  it("should decrease sellIn and quality for normal items", () => {
    const items = [new Item("Normal Item", 10, 20)];
    const shop = new GildedRose(items);

    shop.updateQuality();

    expect(items[0].sellIn).toBe(9);
    expect(items[0].quality).toBe(19);
  });

  it("should decrease quality twice as fast after sellIn <= 0", () => {
    const items = [new Item("Normal Item", 0, 10)];
    const shop = new GildedRose(items);

    shop.updateQuality();

    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(8);
  });

  it("should increase quality of Aged Brie as it gets older", () => {
    const items = [new Item("Aged Brie", 5, 10)];
    const shop = new GildedRose(items);

    shop.updateQuality();

    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(11);
  });

  it("should increase Aged Brie quality twice as fast after sellIn <= 0", () => {
    const items = [new Item("Aged Brie", 0, 10)];
    const shop = new GildedRose(items);

    shop.updateQuality();

    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(12);
  });

  it("should not increase quality above 50", () => {
    const items = [new Item("Aged Brie", 5, 50)];
    const shop = new GildedRose(items);

    shop.updateQuality();

    expect(items[0].quality).toBe(50);
  });

  it("should never decrease quality below 0", () => {
    const items = [new Item("Normal Item", 5, 0)];
    const shop = new GildedRose(items);

    shop.updateQuality();

    expect(items[0].quality).toBe(0);
  });

  it("should not change quality or sellIn for Sulfuras", () => {
    const items = [new Item("Sulfuras, Hand of Ragnaros", 0, 80)];
    const shop = new GildedRose(items);

    shop.updateQuality();

    expect(items[0].sellIn).toBe(0);
    expect(items[0].quality).toBe(80);
  });

  it("should increase Backstage pass quality by 1 when sellIn > 10", () => {
    const items = [
      new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
    ];
    const shop = new GildedRose(items);

    shop.updateQuality();

    expect(items[0].sellIn).toBe(14);
    expect(items[0].quality).toBe(21);
  });

  it("should increase Backstage pass quality by 2 when sellIn <= 10 and > 5", () => {
    const items = [
      new Item("Backstage passes to a TAFKAL80ETC concert", 10, 20),
    ];
    const shop = new GildedRose(items);

    shop.updateQuality();

    expect(items[0].sellIn).toBe(9);
    expect(items[0].quality).toBe(22);
  });

  it("should increase Backstage pass quality by 3 when sellIn <= 5 and > 0", () => {
    const items = [
      new Item("Backstage passes to a TAFKAL80ETC concert", 5, 20),
    ];
    const shop = new GildedRose(items);

    shop.updateQuality();

    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(23);
  });

  it("should drop Backstage pass quality to 0 after the concert (sellIn <= 0)", () => {
    const items = [
      new Item("Backstage passes to a TAFKAL80ETC concert", 0, 30),
    ];
    const shop = new GildedRose(items);

    shop.updateQuality();

    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(0);
  });
});
