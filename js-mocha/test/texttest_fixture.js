const { Shop, Item } = require("../src/gilded_rose");

exports.compose = function (days = 30) {
  const lines = [];

  const items = [
    new Item("+5 Dexterity Vest", 10, 20),
    new Item("Aged Brie", 2, 0),
    new Item("Elixir of the Mongoose", 5, 7),
    new Item("Sulfuras, Hand of Ragnaros", 0, 80),
    new Item("Sulfuras, Hand of Ragnaros", -1, 80),
    new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
    new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
    new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),

    // This Conjured item does not work properly yet
    new Item("Conjured Mana Cake", 3, 13),
  ];

  const gildedRose = new Shop(items);

  lines.push("OMGHAI!");
  for (let day = 0; day < days; day++) {
    lines.push(`\n-------- day ${day} --------`);
    lines.push("name, sellIn, quality");
    items.forEach((item) =>
      lines.push(`${item.name}, ${item.sellIn}, ${item.quality}`)
    );
    gildedRose.updateQuality();
  }

  return lines.join("\n");
};
