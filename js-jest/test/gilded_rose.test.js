const {Shop, Item} = require("../src/gilded_rose");
const gildedRose = new Shop([
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
  ]);

describe("Initial state", function () {
  it("number of items should equal 9", function () {
    expect(gildedRose.items.length).toBe(9);
  });
});

describe("After one updateQuality call", function() {
  it("Miscellaneous item should have decremented sellIn by 1", function() {
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toBe(9);
  });

  it("Miscellaneous item should have decremented quality by 1", function() {
    const items = gildedRose.updateQuality();
    expect(items[0].quality).toBe(19);
  });

  it("Aged Brie should have decremented sellIn by 1", function() {
    const items = gildedRose.updateQuality();
    expect(items[1].sellIn).toBe(1);
  });

  it("Aged Brie should have incremented quality by 1", function() {
    const items = gildedRose.updateQuality();
    expect(items[1].quality).toBe(1);
  });

  // it("should foo", function() {
  //   const items = gildedRose.updateQuality();
  //   expect(items[0].name).toBe("fixme");
  // });
});



