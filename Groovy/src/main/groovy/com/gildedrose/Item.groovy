package com.gildedrose

class Item {

    String name

    int sellIn

    int quality

    Item(String name, int sellIn, int quality) {
        this.name = name
        this.sellIn = sellIn
        this.quality = quality
    }

   @Override
   String toString() {
        return this.name + ", " + this.sellIn + ", " + this.quality
    }
}
