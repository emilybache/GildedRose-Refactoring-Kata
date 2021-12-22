const { Shop, Item } = require("../src/gilded_rose");

describe("Gilded Rose", function () {
  it("should foo", function () {
    const gildedRose = new Shop([new Item("foo", 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toBe("foo");
  });

  it("Aged Brie test", function () {
    const items = [
      new Item("Aged Brie", 2, 0),
    ];
    const gildedRose = new Shop(items);
    for (let day = 0; day < 4; day++) {
      console.log(`\n-------- day ${day} --------`);
      console.log("name, sellIn, quality");
      items.forEach(item => console.log(`${item.name}, ${item.sellIn}, ${item.quality}`));
      gildedRose.updateQuality();
    }
    expect(items[0].name).toBe("Aged Brie");
  });
});
