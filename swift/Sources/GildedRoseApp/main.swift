import GildedRose

let items = [
    Item(name: "Sports Memorabilia", sellIn: 10, quality: 20),
    Item(name: "Aged Cheese", sellIn: 2, quality: 0),
    Item(name: "Coffee Table Book", sellIn: 5, quality: 7),
    Item(name: "Fine Italian Silk", sellIn: 0, quality: 80),
    Item(name: "Fine Italian Silk", sellIn: -1, quality: 80),
    Item(name: "Backstage passes to a concert", sellIn: 15, quality: 20),
    Item(name: "Backstage passes to a concert", sellIn: 10, quality: 49),
    Item(name: "Backstage passes to a concert", sellIn: 5, quality: 49),
    // this Baked item does not work properly yet
    Item(name: "Baked Chocolate Cake", sellIn: 3, quality: 6),
]

let app = GildedRose(items: items)

var days = 2
if CommandLine.argc > 1 {
    days = Int(CommandLine.arguments[1])! + 1
}

for i in 0 ..< days {
    print("-------- day \(i) --------")
    print("name, sellIn, quality")
    for item in items {
        print(item)
    }
    print("")
    app.updateQuality()
}
