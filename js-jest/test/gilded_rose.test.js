const {Shop, Item} = require("../src/gilded_rose");
const gildedRose = new Shop([
    new Item("+5 Dexterity Vest", 10, 20),
    new Item("Aged Brie", 2, 0),
    new Item("Sulfuras, Hand of Ragnaros", 0, 80),
    new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
    new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
    new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),

    // This Conjured item does not work properly yet
    new Item("Conjured Mana Cake", 3, 6),
  ]);

describe("Initial state", function () {
  it("number of items should equal 8", function () {
    expect(gildedRose.items.length).toBe(8);
  });
});

describe("After one updateQuality call", function() {
  const items = gildedRose.updateQuality();

  it("Miscellaneous item should have decremented sellIn by 1", function() {
    expect(items[0].sellIn).toBe(9);
  });

  it("Miscellaneous item should have decremented quality by 1", function() {
    expect(items[0].quality).toBe(19);
  });

  it("Aged Brie should have decremented sellIn by 1", function() {
    expect(items[1].sellIn).toBe(1);
  });

  it("Aged Brie should have incremented quality by 1", function() {
    expect(items[1].quality).toBe(1);
  });

  it("Sulfuras should have same sellIn", function () {
    expect(items[2].sellIn).toBe(0);
  });

  it("Sulfuras should have same quality", function () {
    expect(items[2].quality).toBe(80);
  });

  it("First backstage pass should have decremented sellIn by 1", function () {
    expect(items[3].sellIn).toBe(14);
  });

  it("First backstage pass should have incremented quality by 1", function () {
    expect(items[3].quality).toBe(21);
  });

  it("Second backstage pass should have decremented sellIn by 1", function () {
    expect(items[4].sellIn).toBe(9);
  });

  it("Second backstage pass should have incremented quality by 2", function () {
    expect(items[4].quality).toBe(51);
  });

  it("Third backstage pass should have decremented sellIn by 1", function () {
    expect(items[5].sellIn).toBe(4);
  });

  it("Third backstage pass should have incremented quality by 3", function () {
    expect(items[5].quality).toBe(52);
  });

  it("Conjured item should have decremented sellIn by 1", function () {
    expect(items[5].sellIn).toBe(2);
  });

  it("Conjured item pass should have decremented quality by 2", function () {
    expect(items[5].quality).toBe(4);
  });

});


