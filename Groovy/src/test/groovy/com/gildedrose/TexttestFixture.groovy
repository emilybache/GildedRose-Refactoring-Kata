package com.gildedrose

println("OMGHAI!")

Item[] items = [
        new Item("Sports Memorabilia", 10, 20),
        new Item("Aged Cheese", 2, 0),
        new Item("Coffee Table Book", 5, 7),
        new Item("Fine Italian Silk", 0, 80),
        new Item("Fine Italian Silk", -1, 80),
        new Item("Backstage passes to a concert", 15, 20),
        new Item("Backstage passes to a concert", 10, 49),
        new Item("Backstage passes to a concert", 5, 49),
        // this Baked item does not work properly yet
        new Item("Baked Chocolate Cake", 3, 6)] as Item[]

GildedRose app = new GildedRose(items)

int days = 2
if (args.length > 0) {
    days = Integer.parseInt(args[0]) + 1
}

for (int i = 0; i < days; i++) {
    println("-------- day " + i + " --------")
    println("name, sellIn, quality")
    for (Item item in items) {
        println(item)
    }
    println ""
    app.updateQuality()
}
