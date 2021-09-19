const {Shop, Item, ItemUpdater} = require("../src/gilded_rose");
const {AgedBrieUpdater} = require("../src/item_updaters/aged_brie_updater");

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

  it("Item quality should decrease by two ", function () {
    const gildedRose = new ItemUpdater(new Item("+5 Dexterity Vest", -10, 20));
    const item = gildedRose.updateQuality();
    expect(item.quality).toBe(18);
  });

});

describe("Gilded Rose check Aged Brie rules", function () {
  it("Item quality should increase by one", function () {
    const updater = new AgedBrieUpdater(new Item("Aged Brie", 10, 20));
    const item = updater.updateQuality();
    expect(item.quality).toBe(21);
  });

  it("Item quality should not more than 50", function () {
    const updater = new AgedBrieUpdater(new Item("Aged Brie", 10, 70));
    const item = updater.updateQuality();
    expect(item.quality).toBe(50);
  });

});

describe("Gilded Rose system test", function() {
  it("should check list of itemUpater", function() {
    const itemUpdaters = [
      new ItemUpdater(new Item("+5 Dexterity Vest", 10, 20)),
      new AgedBrieUpdater(new Item("Aged Brie", 2, 0)),
    ];
    
    const days = Number(process.argv[2]) || 2;
    const gildedRose = new Shop(itemUpdaters);
    
    for (let day = 0; day < days; day++) {
      gildedRose.updateQuality();
    }
  });
});