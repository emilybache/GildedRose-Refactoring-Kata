package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {

        // Horrible loop for iterating through items array
        for (int i = 0; i < items.length; i++) {

            // ----- Block 1 ----- Reduce quality of item by 1 for a day if not AB, BptaTc or SHoR
            if (!items[i].name.equals("Aged Brie")
                    && !items[i].name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                if (items[i].quality > 0) {
                    if (!items[i].name.equals("Sulfuras, Hand of Ragnaros")) {
                        items[i].quality = items[i].quality - 1;
                    }
                }
            // If Aged Brie or BptaTc
            } else {
                // Increase quality of item by + 1
                if (items[i].quality < 50) {
                    items[i].quality = items[i].quality + 1;

                    // If pass has less than 11 days to sell, quality increases by + 1
                    if (items[i].name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                        if (items[i].sellIn < 11) {
                            if (items[i].quality < 50) {
                                items[i].quality = items[i].quality + 1;
                            }
                        }

                        // If pass has 6 days to sell, quality increases by + 2
                        if (items[i].sellIn < 6) {
                            if (items[i].quality < 50) {
                                items[i].quality = items[i].quality + 1;
                            }
                        }
                    }
                }
            }

            // ----- Block 2 ------ Item reduces by a day - 1 (unless SHoR)
            if (!items[i].name.equals("Sulfuras, Hand of Ragnaros")) {
                items[i].sellIn = items[i].sellIn - 1;
            }

            // ----- Block 3 ----- Item sell date is less than 0
            if (items[i].sellIn < 0) {

                // Reduce quality of items by - 1 if greater than 0 and not AB, BptaTc or SHoR
                if (!items[i].name.equals("Aged Brie")) {
                    if (!items[i].name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                        if (items[i].quality > 0) {
                            if (!items[i].name.equals("Sulfuras, Hand of Ragnaros")) {
                                items[i].quality = items[i].quality - 1;
                            }
                        }
                    } else {
                        // Item quality goes to 0 of BptaTc as item sell date is less than 0
                        items[i].quality = items[i].quality - items[i].quality;
                    }

                // Increase quality of item if AB
                } else {
                    if (items[i].quality < 50) {
                        items[i].quality = items[i].quality + 1;
                    }
                }
            }
        }
    }
}
