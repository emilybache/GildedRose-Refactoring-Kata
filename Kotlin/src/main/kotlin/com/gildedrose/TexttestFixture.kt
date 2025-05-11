package com.gildedrose

import com.gildedrose.data.constants.ItemRepository

class App(private val glidedRose: GildedRose) {
    fun printGlideRoseTable(days: Int) {
        welcomeMessage()
        repeat(days) { day ->
            printTableHeader(day)
            for (item in glidedRose.items) {
                println("${item.name}, ${item.sellIn}, ${item.quality}")
            }
            println()
            glidedRose.updateQuality()
        }
    }

    private fun printTableHeader(day: Int) {
        println("-------- day $day --------")
        println("name, sellIn, quality")
    }

    private fun welcomeMessage() {
        println("OMGHAI!")
    }
}

//asd
fun main(args: Array<String>) {

    val app = App(GildedRose(ItemRepository.items))

    var days = 2
    if (args.size > 0) {
        days = Integer.parseInt(args[0]) + 1
    }

    app.printGlideRoseTable(days)
}
