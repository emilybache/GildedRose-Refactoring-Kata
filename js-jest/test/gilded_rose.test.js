const {Shop, Item} = require("../src/gilded_rose");

describe("Gilded Rose", function() {
  it("name should be foo", function() {
    const gildedRose = new Shop([new Item("foo", 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toBe("foo");
  });
  it("sellIn should be -1", function() {
    const gildedRose = new Shop([new Item("foo", 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(-1);
  });
  it("foo quality should be 0", function() {
    const gildedRose = new Shop([new Item("foo", 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(0);
  });
  it("foo quality should be 0", function() {
    const gildedRose = new Shop([new Item("foo", 10, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(9);
  });
  it("conjured quality should be 8", function() {
    const gildedRose = new Shop([new Item("Conjured foo", 10, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(8);
  });
  it("conjured quality should be 8", function() {
    const gildedRose = new Shop([new Item("Conjured foo", 10, 1)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(0);
  });
  it("Sulfuras sellIn should be unchanged", function() {
    const gildedRose = new Shop([new Item("Sulfuras, Hand of Ragnaros", 10, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(10);
  });
  it("Sulfuras quality should be unchanged", function() {
    const gildedRose = new Shop([new Item("Sulfuras, Hand of Ragnaros", 10, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(10);
  });
  it("Brie quality should be 11", function() {
    const gildedRose = new Shop([new Item('Aged Brie', 10, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(11);
  });
  it("Brie quality should be 50", function() {
    const gildedRose = new Shop([new Item('Aged Brie', 10, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(50);
  });
  if("Brie quality should be 12", function() {
    const gildedRose = new Shop([new Item('Aged Brie', 0, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(12);
  });
  it("Brie quality should be 50", function() {
    const gildedRose = new Shop([new Item('Aged Brie', 0, 49)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(50);
  });
  it("Backstage pass quality should be 11", function() {
    const gildedRose = new Shop([new Item('Backstage passes to a TAFKAL80ETC concert', 11, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(11);
  });
  it("Backstage pass quality should be 12", function() {
    const gildedRose = new Shop([new Item('Backstage passes to a TAFKAL80ETC concert', 10, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(12);
  });
  it("Backstage pass quality should be 13", function() {
    const gildedRose = new Shop([new Item('Backstage passes to a TAFKAL80ETC concert', 5, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(13);
  });
  it("Backstage pass quality should be 13", function() {
    const gildedRose = new Shop([new Item('Backstage passes to a TAFKAL80ETC concert', 0, 10)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(0);
  });


});
