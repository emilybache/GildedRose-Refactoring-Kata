package com.gildedrose


import java.util.*


fun generateTestCasesInRanger(names: List<String>, sellInRange: IntRange, qualityRange: IntRange): List<Item> {
    val items = LinkedList<Item>()
    for (name in names) {
        for (sellIn in sellInRange) {
            for (quality in qualityRange) {
                items.add(Item(name, sellIn, quality))
            }
        }
    }
    return items
}

fun main(args: Array<String>) {
    val names = listOf(
        com.platinumrose.ItemType.AGED_BRIE.name,
        com.platinumrose.ItemType.BACKSTAGE_PASSES.name,
        com.platinumrose.ItemType.SULFURAS.name,
        "new none-existing on code name"
    )
    val sellInRange = -100..100
    val qualityRange = -100..100
    val allTestCases = generateTestCasesInRanger(names, sellInRange, qualityRange)


    var testCasesCounter = 0
    for (testCase in allTestCases) {
        val app = GildedRose(listOf(Item(testCase.name, testCase.sellIn, testCase.quality)))
        app.updateQuality()

        if (testCase.sellIn != app.items[0].sellIn || testCase.quality != app.items[0].quality) {
            println("$testCasesCounter: $testCase vs ${app.items[0]}")
            testCasesCounter += 1
        }
    }
    println("Found $testCasesCounter testCasesCounter of out ${allTestCases.size} allTestCases")
}
