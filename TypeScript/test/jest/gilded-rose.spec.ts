import { Item, GildedRose } from "@/gilded-rose";

const generalItem = (sellIn: number, quality: number) =>
  new Item("foo", sellIn, quality);
const ageableItem = (sellIn: number, quality: number) =>
  new Item("Aged Brie", sellIn, quality);
const legendaryItem = (sellIn: number, quality: number) =>
  new Item("Sulfuras, Hand of Ragnaros", sellIn, quality);
const backstagePassItem = (sellIn: number, quality: number) =>
  new Item("Backstage passes to a TAFKAL80ETC concert", sellIn, quality);

describe("Gilded Rose", () => {
  it("should degrade quality", () => {
    const gildedRose = new GildedRose([generalItem(1, 1)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(0);
  });
  it("should tick down sell in", () => {
    const gildedRose = new GildedRose([generalItem(1, 1)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(0);
  });
  it("should handle multiple items", () => {
    const gildedRose = new GildedRose([
      new Item("one", 1, 1),
      new Item("two", 2, 3),
    ]);
    const items = gildedRose.updateQuality();
    expect(items.length).toBe(2);
    expect(items[0].name).toBe("one");
    expect(items[0].quality).toBe(0);
    expect(items[0].sellIn).toBe(0);
    expect(items[1].name).toBe("two");
    expect(items[1].quality).toBe(2);
    expect(items[1].sellIn).toBe(1);
  });
  it("should degrade twice as fast past sell in date", () => {
    const gildedRose = new GildedRose([generalItem(0, 3)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(1);
  });
  it("should not have negative quality for expired items", () => {
    const gildedRose = new GildedRose([generalItem(0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(0);
  });
  it("should not have negative quality for non expired items", () => {
    const gildedRose = new GildedRose([generalItem(1, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(0);
    expect(items[0].quality).toBe(0);
  });
  it("should tick up quality for Aged Brie", () => {
    const gildedRose = new GildedRose([ageableItem(1, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(0);
    expect(items[0].quality).toBe(1);
  });
  it("should not exceed a quality of 50", () => {
    const gildedRose = new GildedRose([ageableItem(1, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(0);
    expect(items[0].quality).toBe(50);
  });
  it("should not tick down sell in or quality if the item is Sulfuras", () => {
    const gildedRose = new GildedRose([legendaryItem(1, 2)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(1);
    expect(items[0].quality).toBe(2);
  });
  describe("backstage pass", () => {
    it("should tick up quality with more than 10 days of sell in", () => {
      const gildedRose = new GildedRose([backstagePassItem(11, 2)]);
      const items = gildedRose.updateQuality();
      expect(items[0].sellIn).toBe(10);
      expect(items[0].quality).toBe(3);
    });
    it("should tick up quality twice with less than 10 days of sell in", () => {
      const gildedRose = new GildedRose([backstagePassItem(10, 2)]);
      const items = gildedRose.updateQuality();
      expect(items[0].sellIn).toBe(9);
      expect(items[0].quality).toBe(4);
    });
    it("should tick up quality thrice with less than 5 days of sell in", () => {
      const gildedRose = new GildedRose([backstagePassItem(5, 2)]);
      const items = gildedRose.updateQuality();
      expect(items[0].sellIn).toBe(4);
      expect(items[0].quality).toBe(5);
    });
    it("should have 0 quality when sell in date has passed", () => {
      const gildedRose = new GildedRose([backstagePassItem(0, 2)]);
      const items = gildedRose.updateQuality();
      expect(items[0].sellIn).toBe(-1);
      expect(items[0].quality).toBe(0);
    });
  });
});
