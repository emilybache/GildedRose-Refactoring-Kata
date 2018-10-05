package com.gildedrose.core

import com.gildedrose.GildedRose

fun GildedRose.advanceTimeBy(days: Int) = repeat(days, { this.updateQuality() })
