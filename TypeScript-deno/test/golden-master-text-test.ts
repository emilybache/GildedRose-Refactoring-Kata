import { GildedRose, Item } from "../app/gilded-rose.ts";

export const runner =
  (c: (...args: any[]) => void) => (consoleArgs: string[]) => {
    c("OMGHAI!");

    const items = [
      new Item("+5 Dexterity Vest", 10, 20), //
      new Item("Aged Brie", 2, 0), //
      new Item("Elixir of the Mongoose", 5, 7), //
      new Item("Sulfuras, Hand of Ragnaros", 0, 80), //
      new Item("Sulfuras, Hand of Ragnaros", -1, 80),
      new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
      new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
      new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
      // this conjured item does not work properly yet
      new Item("Conjured Mana Cake", 3, 6),
    ];

    const gildedRose = new GildedRose(items);

    let days: number = 2;
    if (consoleArgs.length > 0) {
      days = +consoleArgs[0];
    }

    for (let i = 0; i < days + 1; i++) {
      c("-------- day " + i + " --------");
      c("name, sellIn, quality");
      items.forEach((element) => {
        c(
          element.name + ", " + element.sellIn + ", " + element.quality,
        );
      });
      c();
      gildedRose.updateQuality();
    }
  };

const main = runner(console.log);
if (import.meta.main) {
  main(Deno.args);
}
