package com.gildedrose;

class GildedRoseAbstract {
    protected Item[] itemsList;

    protected final static AGED_BRIE = "Aged Brie";
    protected final static BACKSTAGE_PASSES = "Backstage passes to a TAFKAL80ETC concert";
    protected final static SULFURAS = "Sulfuras, Hand of Ragnaros";


    public GildedRoseAbstract(Item[] itemsList) {
        this.itemsList = itemsList;
    }

    public abstract void updateQuality();
}
