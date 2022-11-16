package com.gildedrose;

class GildedRoseSulfuras extends GildedRoseAbstract {

    public GildedRoseSulfuras(Item[] itemsList) {
        this.super(itemsList);
    }

    public void updateQuality() {
        for (Item itemValue : itemsList) {
            if (!itemValue.name.equals(SULFURAS)) {
                itemValue.sellIn --;
            }
        }
    }
}
