import {GildedRose, Item} from "../app/gilded-rose";

export const goldenMasterFilename = './build/goldenMaster.txt';

export const getItems = () => ([
    new Item("+5 Dexterity Vest", 10, 20), //
    new Item("Aged Brie", 2, 0), //
    new Item("Elixir of the Mongoose", 5, 7), //
    new Item("Sulfuras, Hand of Ragnaros", 0, 80), //
    new Item("Sulfuras, Hand of Ragnaros", -1, 80),
    new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
    new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
    new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
    // this conjured item does not work properly yet
    new Item("Conjured Mana Cake", 3, 6)
]);

export function generateReport(gildedRose: GildedRose) {
    const outputLines: string[] = []

    const days: number = 2;

    for (let i = 0; i < days; i++) {
        outputLines.push("-------- day " + i + " --------");
        outputLines.push("name, sellIn, quality");
        gildedRose.items.forEach(element => {
            outputLines.push(element.name + ' ' + element.sellIn + ' ' + element.quality);
        });
        outputLines.push();
        gildedRose.updateQuality();
    }

    return outputLines
}