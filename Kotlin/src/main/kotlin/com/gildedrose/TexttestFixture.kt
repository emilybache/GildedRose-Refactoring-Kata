package com.gildedrose

import com.gildedrose.data.constants.ItemRepository

fun main(args: Array<String>) {

    val app = App(GildedRose(ItemRepository.items))

    val days = when {
        args.isNotEmpty() -> Integer.parseInt(args[0]) + 1
        else -> 2
    }

    app.printGlideRoseTable(days)
}