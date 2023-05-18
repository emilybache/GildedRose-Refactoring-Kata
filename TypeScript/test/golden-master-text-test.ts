import { Item, GildedRose } from '../app/gilded-rose';

const items = [
  new Item("Sports Memorabilia", 10, 20), //
  new Item("Aged Cheese", 2, 0), //
  new Item("Coffee Table Book", 5, 7), //
  new Item("Fine Italian Silk", 0, 80), //
  new Item("Fine Italian Silk", -1, 80),
  new Item("Backstage passes to a concert", 15, 20),
  new Item("Backstage passes to a concert", 10, 49),
  new Item("Backstage passes to a concert", 5, 49),
  // this Baked item does not work properly yet
  new Item("Baked Chocolate Cake", 3, 6)];


const gildedRose = new GildedRose(items);

let days: number = 2;
if (process.argv.length > 2) {
    days = +process.argv[2];
  }

for (let i = 0; i < days; i++) {
  console.log("-------- day " + i + " --------");
  console.log("name, sellIn, quality");
  items.forEach(element => {
    console.log(element.name + ' ' + element.sellIn + ' ' + element.quality);

  });
  console.log();
  gildedRose.updateQuality();
}
