const {Shop, Item, ItemUpdater} = require("../src/gilded_rose");

describe("Gilded Rose", function() {
  it("should foo", function() {
    const gildedRose = new Shop([new Item("foo", 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toBe("foo");
  });
});

describe("Gilded Rose check common rules", function () {
  it("Item quality should decrease by one", function () {
    const gildedRose = new ItemUpdater(new Item("+5 Dexterity Vest", 10, 20));
    const item = gildedRose.updateQuality();
    expect(item.quality).toBe(19);
  });

  it("Item quality should not decrease by one", function () {
    const gildedRose = new ItemUpdater(new Item("+5 Dexterity Vest", 10, 0));
    const item = gildedRose.updateQuality();
    expect(item.quality).toBe(0);
  });

});