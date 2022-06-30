const {Shop, Item} = require("../src/gilded_rose");

describe("Normal Item", function() {
  it("before sell date", function() {
    const gildedRose = new Shop([new Item("normal", 5, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(9);
  });

  it("on sell date", function() {
    const gildedRose = new Shop([new Item("normal", 0, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(8);
  });

  it("after sell date", function() {
    const gildedRose = new Shop([new Item("normal", -5, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-6);
    expect(items[0].quality).toBe(8);
  });

  it("with a quality of 0", function() {
    const gildedRose = new Shop([new Item("normal", 5, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(0);
  });
});

describe("Aged Brie Item", function () {
  it("before sell date", function() {
    const gildedRose = new Shop([new Item("Aged Brie", 5, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(11);
  });

  it("before sell date with maximum quality", function() {
    const gildedRose = new Shop([new Item("Aged Brie", 5, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(50);
  });

  it("on sell date", function() {
    const gildedRose = new Shop([new Item("Aged Brie", 0, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(12);
  });

  it("on sell date near maximum quality", function() {
    const gildedRose = new Shop([new Item("Aged Brie", 0, 49)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(50);
  });

  it("on sell date with maximum quality", function() {
    const gildedRose = new Shop([new Item("Aged Brie", 0, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(50);
  });

  it("after sell date", function() {
    const gildedRose = new Shop([new Item("Aged Brie", -10, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-11);
    expect(items[0].quality).toBe(12);
  });

  it("after sell date with maximum quality", function() {
    const gildedRose = new Shop([new Item("Aged Brie", -10, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-11);
    expect(items[0].quality).toBe(50);
  });
});

describe("Sulfuras item", function () {
  it("before sell date", function() {
    const gildedRose = new Shop([new Item("Sulfuras, Hand of Ragnaros", 5, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(5);
    expect(items[0].quality).toBe(10);
  });

  it("on sell date", function() {
    const gildedRose = new Shop([new Item("Sulfuras, Hand of Ragnaros", 0, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(0);
    expect(items[0].quality).toBe(10);
  });

  it("after sell date", function() {
    const gildedRose = new Shop([new Item("Sulfuras, Hand of Ragnaros", -1, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(10);
  });
});

describe("Backstage Pass", function () {
  it("long before sell date", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 11, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(10);
    expect(items[0].quality).toBe(11);
  });

  it("close to before sell date", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 10, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(9);
    expect(items[0].quality).toBe(12);
  });

  it("close to sell date at maximum quality", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 10, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(9);
    expect(items[0].quality).toBe(50);
  });

  it("very close to sell date", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 5, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(13);
  });

  it("very close to sell date at maximum quality", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 5, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(50);
  });

  it("with 1 day left to sell date at maximum quality", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 1, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(0);
    expect(items[0].quality).toBe(50);
  });

  it("on sell date", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", 0, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(0);
  });

  it("after sell date", function() {
    const gildedRose = new Shop([new Item("Backstage passes to a TAFKAL80ETC concert", -1, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-2);
    expect(items[0].quality).toBe(0);
  });
});

