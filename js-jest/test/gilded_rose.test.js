const { Shop, Item } = require("../src/gilded_rose");
// require("./texttest_fixture");

describe("Gilded Rose", function () {
  let gildedRose;
  beforeEach(function () {
    const items = [
      new Item("+5 Dexterity Vest", 10, 20),
      new Item("Aged Brie", 2, 0),
      new Item("Elixir of the Mongoose", 5, 7),
      new Item("Sulfuras, Hand of Ragnaros", 0, 80),
      new Item("Sulfuras, Hand of Ragnaros", -1, 80),
      new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
      new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
      new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),

      // This Conjured item does not work properly yet
      new Item("Conjured Mana Cake", 3, 6),
    ];
    gildedRose = new Shop(items);
  });
  it("should preserve the name of items when updating quality", function () {
    const gildedRose = new Shop([new Item("foo", 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toBe("foo");
  });
  describe("plain items", function () {
    it("should decrease quality every day", function () {
      gildedRose.updateQuality();
      const updatedItems = gildedRose.updateQuality();
      expect(updatedItems[0].quality).toBe(18);
    });
    it("should decrease sellIn days every day", function () {
      gildedRose.updateQuality();
      const updatedItems = gildedRose.updateQuality();
      expect(updatedItems[0].sellIn).toBe(8);
    });
  });
  describe("aged Bried", function () {
    it("should increase quality every day", function () {
      gildedRose.updateQuality();
      const items = gildedRose.updateQuality();
      expect(items[1].quality).toBe(2);
    });

    it("should decrease sellIn days every day", function () {
      gildedRose.updateQuality();
      const updatedItems = gildedRose.updateQuality();
      expect(updatedItems[1].sellIn).toBe(0);
    });
  });
  describe("Sulfuras", function () {
    it("should never decrease in quality", function () {
      gildedRose.updateQuality();
      const items = gildedRose.updateQuality();
      expect(items[3].quality).toBe(80);
      expect(items[4].quality).toBe(80);
    });

    it("should never decrease sellIn days", function () {
      const items = gildedRose.updateQuality();
      expect(items[3].sellIn).toBe(0);
      expect(items[4].sellIn).toBe(-1);
    });
  });
  describe("Backstage passes", function () {
    it("should increase in quality by 3 every day when there are 5 days or less left", function () {
      gildedRose.updateQuality();
      const items = gildedRose.updateQuality();
      expect(items[7].quality).toBe(55);
    });
    it("should increase in quality by 2 every day when there are between 6 and 10 days left", function () {
      gildedRose.updateQuality();
      const items = gildedRose.updateQuality();
      expect(items[6].quality).toBe(53);
    });
    it("should increase in quality by 1 every day when there are more than 10 days left", function () {
      gildedRose.updateQuality();
      const items = gildedRose.updateQuality();
      expect(items[5].quality).toBe(22);
    });
    it("should drop quality to zero after the concert", function () {
      gildedRose.updateQuality();
      gildedRose.updateQuality();
      gildedRose.updateQuality();
      gildedRose.updateQuality();
      const items = gildedRose.updateQuality();
      expect(items[7].sellIn).toBe(0);
      expect(items[7].quality).toBe(0);
    });

    it("should decrease sellIn days every day", function () {
      gildedRose.updateQuality();
      const items = gildedRose.updateQuality();
      expect(items[5].sellIn).toBe(13);
      expect(items[6].sellIn).toBe(8);
      expect(items[7].sellIn).toBe(3);
    });
  });

  describe("Conjured", function () {
    it("should increase in quality by 2 every day", function () {
      gildedRose.updateQuality();
      const items = gildedRose.updateQuality();
      expect(items[8].quality).toBe(2);
    });

    it("should decrease sellIn days every day", function () {
      gildedRose.updateQuality();
      const items = gildedRose.updateQuality();
      expect(items[8].sellIn).toBe(1);
    });
  });
});
