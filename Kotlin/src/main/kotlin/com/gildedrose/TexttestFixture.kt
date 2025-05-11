package com.gildedrose

fun main(args: Array<String>) {

    val app = App()

    val days = when {
        args.isNotEmpty() -> Integer.parseInt(args[0]) + 1
        else -> 2
    }

    app.printGlideRoseTable(days)
}