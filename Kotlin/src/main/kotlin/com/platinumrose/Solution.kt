package com.platinumrose

import com.gildedrose.Item

interface Solution {
    fun items(): List<Item>
    fun updateQuality()
}