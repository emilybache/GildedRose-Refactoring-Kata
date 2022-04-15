const {Shop, Item} = require("../src/gilded_rose");

describe("Gilded Rose with SellIn Zero", function() {
  it("Check Sulfuras quantity", function() {
    const gildedRose = new Shop([new Item("Sulfuras", 0, 78)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(80);
  })

  it("Aged Brie to increase if SellIn is 0", function() {
    const gildedRose = new Shop([new Item("Aged Brie", 0, 20)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(21);
  });

  it("Other item to decrease twice if SellIn is 0", function() {
    const gildedRose = new Shop([new Item("Other item 1", 0, 20)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(18);
  });

  it("Conjured to decrease twice of normal items if SellIn is 0", function() {
    const gildedRose = new Shop([new Item("Conjured", 0, 20)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(16);
  });

  it("Backstage passes to increase if SellIn is 0", function() {
    const gildedRose = new Shop([new Item("Backstage passes", 0, 20)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(21);
  });

});


describe("Gilded Rose with SellIn Non Zero", function() {
  it("Check Sulfuras quantity", function() {
    const gildedRose = new Shop([new Item("Sulfuras", 2, 78)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(80);
  })

  it("Aged Brie to increase if SellIn is 4", function() {
    const gildedRose = new Shop([new Item("Aged Brie", 4, 20)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(23);
  });

  it("Other item quantity no change if SellIn is greater than 0", function() {
    const gildedRose = new Shop([new Item("Other item 1", 5, 20)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(20);
  });

  it("Conjured quantity no change if SellIn is greater than 0", function() {
    const gildedRose = new Shop([new Item("Conjured", 6, 20)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(20);
  });

  it("Backstage passes to increase if SellIn is 8", function() {
    const gildedRose = new Shop([new Item("Backstage passes", 8, 20)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(22);
  });

});