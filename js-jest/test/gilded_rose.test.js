const {Shop, Item} = require("../src/gilded_rose");

describe("default item", function() {
  it("should decrease quality over time by 1", function() {
    const gildedRose = new Shop([new Item("default", 10, 2)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(1);
  });

  it("should decrease quality by 2 when sellIn < 0", function() {
    const gildedRose = new Shop([new Item("default", 0, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(8);
  });

  it("should decrease sellIn over time by 1", function() {
    const gildedRose = new Shop([new Item("default", 10, 2)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(9);
  });

  it("should never drop quality below 0", function() {
    const gildedRose = new Shop([new Item("default", 10, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(0);
  });
});

describe("Aged Brie", function() {
  it("should increase quality over time by 1", function() {
    const gildedRose = new Shop([new Item("Aged Brie", 10, 2)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(3);
  });

  it("should increase quality by 2 when sellIn < 0", function() {
    const gildedRose = new Shop([new Item("Aged Brie", 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(2);
  });

  it("should stay at 50 quality max", function() {
    const gildedRose = new Shop([new Item("Aged Brie", 0, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(50);
  });
});

describe("Backstage passes to a TAFKAL80ETC concert", function() {
  it("should increase quality over time by 1 if sellin > 10", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 20, 2)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(3);
  });

  it("should increase quality by 2 when sellIn < 11", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 10, 2)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(4);
  });

  it("should not increase quality over 50 when sellIn < 11", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(50);
  });

  it("should increase quality by 3 when sellIn < 6", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 5, 2)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(5);
  });

  it("should not increase quality over 50 when sellIn < 6", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 5, 48)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(50);
  });

  it("should have 0 quality when sellIn < 0", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 0, 2)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(0);
  });
});


describe("Sulfuras, Hand of Ragnaros", function() {
  it("quality never changes", function() {
    const gildedRose = new Shop([new Item("Sulfuras, Hand of Ragnaros", 20, 80)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(80);
  });

  it("quality never changes even if sellIn < 0", function() {
    const gildedRose = new Shop([new Item("Sulfuras, Hand of Ragnaros", -1, 80)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(80);
  });

  it("sellIn never changes", function() {
    const gildedRose = new Shop([new Item("Sulfuras, Hand of Ragnaros", 20, 80)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(20);
  });

  // it("sellIn never changes", function() {
  //   const gildedRose = new Shop([new Item("Sulfuras, Hand of Ragnaros", 20, 80)]);
  //   const items = gildedRose.updateQuality();
  //   expect(items[0].sellIn).toBe(20);
  // });
});
