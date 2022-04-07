const { Shop, Item } = require("../src/gilded_rose");

describe("Gilded Rose", function () {
  it("Quality never is negative", function () {
    const gildedRose = new Shop([new Item("foo", 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(0);
  });

  it("Sulfuras could not be sold", function () {
    const gildedRose = new Shop([
      new Item("Sulfuras, Hand of Ragnaros", 10, 0),
    ]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(10);
  });

  it("Sulfuras could not decrease quality", function () {
    const gildedRose = new Shop([
      new Item("Sulfuras, Hand of Ragnaros", 10, 10),
    ]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(10);
  });

  it("Quality could not be more than fifty", function () {
    const gildedRose = new Shop([new Item("Aged Brie", 10, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(50);
  });

  it("Item with date passed quality decrease by twice", function () {
    const gildedRose = new Shop([new Item("foo", -1, 40)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(38);
  });

  it("Aged brie increase quality when it gets older", function () {
    const gildedRose = new Shop([new Item("Aged Brie", 1, 40)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(41);
  });

  it("Aged brie increase by two quality when date passed", function () {
    const gildedRose = new Shop([new Item("Aged Brie", -1, 40)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(42);
  });

  it("Aged brie increase by two quality when date passed and not more than fifty", function () {
    const gildedRose = new Shop([new Item("Aged Brie", -1, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(50);
  });

  it("Backstage passes increase quality by two when sell-in less than ten", function () {
    const gildedRose = new Shop([
      new Item("Backstage passes to a TAFKAL80ETC concert", 10, 40),
    ]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(42);
  });

  it("Backstage passes increase quality by two when sell-in less than six", function () {
    const gildedRose = new Shop([
      new Item("Backstage passes to a TAFKAL80ETC concert", 6, 40),
    ]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(42);
  });

  it("Backstage passes increase quality by three when sell-in less than five", function () {
    const gildedRose = new Shop([
      new Item("Backstage passes to a TAFKAL80ETC concert", 5, 40),
    ]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(43);
  });

  it("Backstage passes increase quality by two when sell-in less than six and not more than fifty", function () {
    const gildedRose = new Shop([
      new Item("Backstage passes to a TAFKAL80ETC concert", 6, 49),
    ]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(50);
  });

  it("Backstage passes increase quality by three when sell-in less than five and not more than fifty", function () {
    const gildedRose = new Shop([
      new Item("Backstage passes to a TAFKAL80ETC concert", 5, 48),
    ]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(50);
  });

  it("Backstage passes quality drops to zero after concert", function () {
    const gildedRose = new Shop([
      new Item("Backstage passes to a TAFKAL80ETC concert", 0, 40),
    ]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(0);
  });

  it("Backstage passes quality increase quality by one when date is more than 10", function () {
    const gildedRose = new Shop([
      new Item("Backstage passes to a TAFKAL80ETC concert", 11, 40),
    ]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(41);
  });
});
