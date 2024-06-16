package com.gildedrose

// NOTE: changes to data class to make comparison (asserts) easier. If rough goblin is against it, I'm ready to die
data class Item(var name: String, var sellIn: Int, var quality: Int) {
    override fun toString(): String {
        return this.name + ", " + this.sellIn + ", " + this.quality
    }
}