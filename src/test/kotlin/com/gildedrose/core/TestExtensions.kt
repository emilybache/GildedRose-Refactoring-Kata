package com.gildedrose.core

import com.gildedrose.Item

fun List<Item>.excludeSulfuras() = this.filter { it.name != "Sulfuras, Hand of Ragnaros" }
fun List<Item>.excludeBackstagePasses() = this.filter { it.name != "Backstage passes to a TAFKAL80ETC concert" }
fun List<Item>.excludeAgedBrie() = this.filter { it.name != "Aged Brie" }