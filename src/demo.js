
const { Shop, Item } = require("./gilded_rose");

const items = [
  new Item("+5 Dexterity Vest", 10, 20),
  new Item("Aged Brie", 2, 0),
  new Item("Elixir of the Mongoose", 5, 7),
  new Item("Sulfuras", 0, 80),
  new Item("Sulfuras", -1, 80),
  new Item("Backstage passes", 15, 20),
  new Item("Backstage passes", 10, 49),
  new Item("Backstage passes", 5, 49),

  // This Conjured item does not work properly yet
  new Item("Conjured Mana Cake", 3, 6),
];

const days = Number(process.argv[2]) || 3;
const gildedRose = new Shop(items);

console.log("Welcome to the Gilded Rose Demo!");
console.log("================================")

for (let day = 1; day <= days; day++) {
  console.log(`\n-------- Day ${day} --------`);
  console.log("name, sellIn, quality");
  items.forEach(item => console.log(`${item.name}, ${item.sellIn}, ${item.quality}`));
  gildedRose.updateQuality();
}
