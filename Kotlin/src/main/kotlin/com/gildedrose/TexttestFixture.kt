package com.gildedrose

import com.gildedrose.data.constants.ItemRepository

fun main(args: Array<String>) {

    val app = App(GildedRose(ItemRepository.items))

    var days = 2
    if (args.size > 0) {
        days = Integer.parseInt(args[0]) + 1
    }

    app.printGlideRoseTable(days)
}