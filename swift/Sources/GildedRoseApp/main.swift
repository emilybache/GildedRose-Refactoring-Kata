import GildedRose

let items = [
    Item(name: "+5 Dexterity Vest", sellIn: 10, quality: 20),
    Item(name: "Aged Brie", sellIn: 2, quality: 0),
    Item(name: "Elixir of the Mongoose", sellIn: 5, quality: 7),
    Item(name: "Sulfuras, Hand of Ragnaros", sellIn: 0, quality: 80),
    Item(name: "Sulfuras, Hand of Ragnaros", sellIn: -1, quality: 80),
    Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 15, quality: 20),
    Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 10, quality: 49),
    Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 5, quality: 49),
    // this conjured item does not work properly yet
    Item(name: "Conjured Mana Cake", sellIn: 3, quality: 6)]

let app = GildedRose(items: items);

var days = 2;
if (CommandLine.argc > 1) {
    days = Int(CommandLine.arguments[1])! + 1
}

for i in 0..<days {
    print("-------- day \(i) --------");
    print("name, sellIn, quality");
    for item in items {
        print(item);
    }
    print("");
    app.updateQuality();
}
