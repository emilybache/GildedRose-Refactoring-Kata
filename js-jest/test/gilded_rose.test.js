const {Shop, Item, ItemUpdater} = require("../src/gilded_rose");

describe("Gilded Rose", function() {
  it("should foo", function() {
    const gildedRose = new Shop([new Item("foo", 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toBe("foo");
  });
});

describe("Gilded Rose check common rules", function () {
  it("should foo", function () {
    const gildedRose = new ItemUpdater(new Item("Aged Brie", 10, 0));
    const item = gildedRose.updateQuality();
    expect(item.quality).toBe(10);
  });
});
