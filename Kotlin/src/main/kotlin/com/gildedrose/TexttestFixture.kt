package com.gildedrose

import com.gildedrose.data.constants.ItemRepository


//asd
fun main(args: Array<String>) {

    println("OMGHAI!")

    val app = GildedRose(ItemRepository.items)

    var days = 2
    if (args.size > 0) {
        days = Integer.parseInt(args[0]) + 1
    }

    for (i in 0..days - 1) {
        println("-------- day $i --------")
        println("name, sellIn, quality")
        for (item in app.items) {
            println("${item.name}, ${item.sellIn}, ${item.quality}")
        }
        println()
        app.updateQuality()
    }
}
