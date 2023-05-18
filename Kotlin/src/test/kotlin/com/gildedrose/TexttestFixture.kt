package com.gildedrose

fun main(args: Array<String>) {

    println("OMGHAI!")

    val items = listOf(Item("Sports Memorabilia", 10, 20), //
            Item("Aged Cheese", 2, 0), //
            Item("Coffee Table Book", 5, 7), //
            Item("Fine Italian Silk", 0, 80), //
            Item("Fine Italian Silk", -1, 80),
            Item("Backstage passes to a concert", 15, 20),
            Item("Backstage passes to a concert", 10, 49),
            Item("Backstage passes to a concert", 5, 49),
            // this Baked item does not work properly yet
            Item("Baked Chocolate Cake", 3, 6))

    val app = GildedRose(items)

    var days = 2
    if (args.size > 0) {
        days = Integer.parseInt(args[0]) + 1
    }

    for (i in 0..days - 1) {
        println("-------- day $i --------")
        println("name, sellIn, quality")
        for (item in items) {
            println(item)
        }
        println()
        app.updateQuality()
    }
}
