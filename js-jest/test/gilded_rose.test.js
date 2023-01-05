const {Shop, Item} = require("../src/gilded_rose");

const gildedRose = new Shop([
    new Item("+5 Dexterity Vest", 10, 20),
    new Item("Aged Brie", 2, 0),
    new Item("Sulfuras, Hand of Ragnaros", 0, 35),
    new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
    new Item("Backstage passes to a TAFKAL80ETC concert", 10, 45),
    new Item("Backstage passes to a TAFKAL80ETC concert", 5, 32),
    new Item("Conjured Mana Cake", 3, 6),
    new Item("Expired Pears", -1, 11),
    new Item("Overpriced Trinket", 10, 55),
    new Item("Conjured Mana Cake", -3, 6),
  ]);

const items = gildedRose.updateQuality();

describe("After one updateQuality call", function() {
  
  it("Miscellaneous item should have decremented sellIn by 1", function() {
    expect(items[0].sellIn).toBe(9);
  });

  it("Miscellaneous item should have decremented quality by 1", function() {
    expect(items[0].quality).toBe(19);
  });

  it("Expired item should have decremented quality by 2", function () {
    expect(items[7].quality).toBe(9);
  });

  it("Overpriced item should have reset quality to 50", function () {
    expect(items[8].quality).toBe(50);
  });

});

describe("After one updateQuality call, Aged Brie", function () {
  it("should have decremented sellIn by 1", function () {
    expect(items[1].sellIn).toBe(1);
  });

  it("should have incremented quality by 1", function () {
    expect(items[1].quality).toBe(1);
  });
});

describe("After one updateQuality call, Sulfuras", function () {
  it("should have same sellIn", function () {
    expect(items[2].sellIn).toBe(0);
  });

  it("should have same quality", function () {
    expect(items[2].quality).toBe(35);
  });
});

describe("After one updateQuality call", function () {
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
    expect(items[4].quality).toBe(47);
  });

  it("Third backstage pass should have decremented sellIn by 1", function () {
    expect(items[5].sellIn).toBe(4);
  });

  it("Third backstage pass should have incremented quality by 3", function () {
    expect(items[5].quality).toBe(35);
  });
});

describe("After one updateQuality call, Conjured item", function () {
  it("should have decremented sellIn by 1", function () {
    expect(items[6].sellIn).toBe(2);
  });

  it("should have decremented quality by 2", function () {
    expect(items[6].quality).toBe(4);
  });

  it("should have decremented quality by 4 if expired", function () {
    expect(items[9].quality).toBe(2);
  });
});