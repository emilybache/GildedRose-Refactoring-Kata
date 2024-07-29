import { Item, GildedRose } from "@/gilded-rose";

describe("Gilded Rose", () => {
  describe("Normal items", () => {
    it("should decrease quality by 1", () => {
      const gildedRose = new GildedRose([new Item("foo", 1, 1)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(0);
    });

    it("should decrease sellIn by 1", () => {
      const gildedRose = new GildedRose([new Item("foo", 1, 1)]);
      const items = gildedRose.updateQuality();
      expect(items[0].sellIn).toBe(0);
    });

    it("should decrease quality by 2 after sellIn", () => {
      const gildedRose = new GildedRose([new Item("foo", 0, 2)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(0);
    });

    it("should not decrease quality below 0", () => {
      const gildedRose = new GildedRose([new Item("foo", 1, 0)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(0);
    });

    it("should not decrease quality below 0 after sellIn", () => {
      const gildedRose = new GildedRose([new Item("foo", 0, 0)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(0);
    });
  });

  describe("Aged Brie", () => {
    it("should increase quality by 1", () => {
      const gildedRose = new GildedRose([new Item("Aged Brie", 1, 1)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(2);
    });

    it("should increase quality by 2 after sellIn", () => {
      const gildedRose = new GildedRose([new Item("Aged Brie", 0, 1)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(3);
    });
  });

  describe("Sulfuras", () => {
    it("should not change quality", () => {
      const gildedRose = new GildedRose([
        new Item("Sulfuras, Hand of Ragnaros", 1, 80),
      ]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(80);
    });

    it("should not change sellIn", () => {
      const gildedRose = new GildedRose([
        new Item("Sulfuras, Hand of Ragnaros", 1, 80),
      ]);
      const items = gildedRose.updateQuality();
      expect(items[0].sellIn).toBe(1);
    });
  });

  describe("Backstage passes", () => {
    it("should increase quality by 1", () => {
      const gildedRose = new GildedRose([
        new Item("Backstage passes to a TAFKAL80ETC concert", 11, 1),
      ]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(2);
    });

    it("should increase quality by 2 when 10 days or less", () => {
      const gildedRose = new GildedRose([
        new Item("Backstage passes to a TAFKAL80ETC concert", 10, 1),
      ]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(3);
    });

    it("should increase quality by 3 when 5 days or less", () => {
      const gildedRose = new GildedRose([
        new Item("Backstage passes to a TAFKAL80ETC concert", 5, 1),
      ]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(4);
    });

    it("should drop quality to 0 after sellIn", () => {
      const gildedRose = new GildedRose([
        new Item("Backstage passes to a TAFKAL80ETC concert", 0, 1),
      ]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(0);
    });
  });

  describe("Conjured items", () => {
    it("should decrease quality by 2", () => {
      const gildedRose = new GildedRose([new Item("Conjured", 1, 2)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(0);
    });

    it("should decrease quality by 4 after sellIn", () => {
      const gildedRose = new GildedRose([new Item("Conjured", 0, 4)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toBe(0);
    });
  });
});
