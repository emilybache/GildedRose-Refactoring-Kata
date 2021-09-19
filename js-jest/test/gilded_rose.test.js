const {Shop, Item, ItemUpdater} = require("../src/gilded_rose");

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

describe("Gilded Rose system test", function() {
  it("should check list of itemUpater", function() {
    const itemUpdaters = [
      new ItemUpdater(new Item("+5 Dexterity Vest", 10, 20)),
    ];
    
    const days = Number(process.argv[2]) || 2;
    const gildedRose = new Shop(itemUpdaters);
    
    for (let day = 0; day < days; day++) {
      gildedRose.updateQuality();
    }
  });
});