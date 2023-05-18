
const { Shop, Item } = require("../src/gilded_rose");

const items = [
  new Item("Sports Memorabilia", 10, 20),
  new Item("Aged Cheese", 2, 0),
  new Item("Coffee Table Book", 5, 7),
  new Item("Fine Italian Silk", 0, 80),
  new Item("Fine Italian Silk", -1, 80),
  new Item("Backstage passes to a concert", 15, 20),
  new Item("Backstage passes to a concert", 10, 49),
  new Item("Backstage passes to a concert", 5, 49),

  // This Baked item does not work properly yet
  new Item("Baked Chocolate Cake", 3, 6),
];

const days = Number(process.argv[2]) || 2;
const gildedRose = new Shop(items);

console.log("OMGHAI!");
for (let day = 0; day < days; day++) {
  console.log(`\n-------- day ${day} --------`);
  console.log("name, sellIn, quality");
  items.forEach(item => console.log(`${item.name}, ${item.sellIn}, ${item.quality}`));
  gildedRose.updateQuality();
}
