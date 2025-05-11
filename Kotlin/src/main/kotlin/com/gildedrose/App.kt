package com.gildedrose

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