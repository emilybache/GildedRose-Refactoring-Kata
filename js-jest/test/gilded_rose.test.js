const {Shop, Item} = require("../src/gilded_rose");
const {AgedBrieUpdater} = require("../src/item_updaters/aged_brie_updater");
const {SulfurasUpdater } = require("../src/item_updaters/sulfuras_updater");
const {BackStagePassesUpdater } = require("../src/item_updaters/backstage_passes_updater");
const {ConjuredUpdater } = require("../src/item_updaters/conjured_updater");
const {ItemUpdater } = require("../src/item_updater");

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
    const updater = new AgedBrieUpdater(new Item("Aged Brie", 2, 0));
    const item = updater.updateQuality();
    expect(item.quality).toBe(0);
  });

  it("Item quality should not more than 50", function () {
    const updater = new AgedBrieUpdater(new Item("Aged Brie", 10, 70));
    const item = updater.updateQuality();
    expect(item.quality).toBe(50);
  });

});

describe("Gilded Rose check Sulfuras rules", function () {
  it("Item quality should be 80", function () {
    const updater = new SulfurasUpdater(new Item("Sulfuras, Hand of Ragnaros", 0, 80));
    const item = updater.updateQuality();
    expect(item.quality).toBe(80);
  });

  it("Item quality should be 80", function () {
    const updater = new SulfurasUpdater(new Item("Sulfuras, Hand of Ragnaros", -1, 80));
    const item = updater.updateQuality();
    expect(item.quality).toBe(80);
  });

});

describe("Gilded Rose check BackStage Passes rules", function () {
  it("Item quality should be increase by one", function () {
    const updater = new BackStagePassesUpdater(new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20));
    const item = updater.updateQuality();
    expect(item.quality).toBe(21);
  });

  it("Item quality should be increase by two", function () {
    const updater = new BackStagePassesUpdater(new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49));
    const item = updater.updateQuality();
    expect(item.quality).toBe(51);
  });

  it("Item quality should be increase by three", function () {
    const updater = new BackStagePassesUpdater(new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49));
    const item = updater.updateQuality();
    expect(item.quality).toBe(52);
  });

});

describe("Gilded Rose check Conjured rules", function () {
  it("Item quality should be decrease by 2", function () {
    const updater = new ConjuredUpdater(new Item("Conjured Mana Cake", 3, 6));
    const item = updater.updateQuality();
    expect(item.quality).toBe(4);
  });

});

describe("Gilded Rose system test", function() {
  it("should check list of itemUpaters", function () {
    const itemUpdaters = [
      new ItemUpdater(new Item("+5 Dexterity Vest", 10, 20)),
      new AgedBrieUpdater(new Item("Aged Brie", 2, 0)),
      new ItemUpdater(new Item("Elixir of the Mongoose", 5, 7)),
      new SulfurasUpdater(new Item("Sulfuras, Hand of Ragnaros", 0, 80)),
      new SulfurasUpdater(new Item("Sulfuras, Hand of Ragnaros", -1, 80)),
      new BackStagePassesUpdater(new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20)),
      new BackStagePassesUpdater(new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49)),
      new BackStagePassesUpdater(new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49)),
      new ConjuredUpdater(new Item("Conjured Mana Cake", 3, 6)),
    ];

    const days = Number(process.argv[2]) || 2;
    const gildedRose = new Shop(itemUpdaters);
    
    for (let day = 0; day < days; day++) {
      gildedRose.updateQuality();
    }
  });
});