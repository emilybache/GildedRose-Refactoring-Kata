package com.gildedrose;

class GildedRose {

    private final String AGED_BRIE = "Aged Brie";
    private final String BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert";
    private final String SULFURAS = "Sulfuras, Hand of Ragnaros";

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : this.items) {
            // update each item individually
            switch (item.name) {
                case AGED_BRIE:
                    updateAgedBrie(item);
                    continue;

                case BACKSTAGE_PASS:
                    updateBackStage(item);
                    continue;
                case SULFURAS:
                    updateSulfuras(item);
                    continue;
                default:
                    updateNormalItem(item);
                    continue;
            }
        }
    }

    public void updateAgedBrie(Item item) {

        // quality for any item can not exceed 50
        if (item.quality < 50) {
            item.quality = item.quality + 1; // age brie quality update


        }
        // update sell in date
        item.sellIn = item.sellIn - 1;
        // quality for age brie gets better as it ages past its sellIn
        if (item.sellIn < 0 && item.quality < 50) {
            item.quality = item.quality + 1; // age brie quality update
        }
    }

    public void updateBackStage(Item item) {
        // no items quality gets above 50
        if (item.quality < 50) {
            item.quality = item.quality + 1; // backstage quality update

            // quality increases twice if 10 days to sellIn
            if (item.sellIn < 11 && item.quality < 50) {
                item.quality = item.quality + 1;
            }
            // quality increases by three if 5 days to sellIn
            if (item.sellIn < 6 && item.quality < 50) {
                item.quality = item.quality + 1;
            }
        }
        // update sell in date for Backstage item
        item.sellIn = item.sellIn - 1;
        // quality drops to zero if it's past concert date
        if (item.sellIn < 0) {
            item.quality = item.quality - item.quality;
        }
    }

    public void updateSulfuras(Item item) {
        // Sulfuras is a legendary item
        // nothing happens for the sulfuras Item
    }

    public void updateNormalItem(Item item) {

        // normal item quality decreases by 1 every day
        if (item.quality > 0) {
            item.quality = item.quality - 1;
        }

        // update sell in date for Normal item
        item.sellIn = item.sellIn - 1;

        // quality drops twice if item is past sellIn Date
        if (item.sellIn < 0 && item.quality > 0) {
            item.quality = item.quality - 1;
        }
    }



}
