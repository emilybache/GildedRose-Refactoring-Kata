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
            Item item = items[i];
            if (!item.name.equals("Aged Brie")
                    && !item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                if (item.quality > 0) {
                    if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                        item.quality = item.quality - 1;
                    }
                }
            // If Aged Brie or BptaTc
            } else {
                // Increase quality of item by + 1
                if (item.quality < 50) {
                    item.quality = item.quality + 1;

                    // If pass has less than 11 days to sell, quality increases by + 1
                    if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                        if (item.sellIn < 11) {
                            if (item.quality < 50) {
                                item.quality = item.quality + 1;
                            }
                        }

                        // If pass has 6 days to sell, quality increases by + 2
                        if (item.sellIn < 6) {
                            if (item.quality < 50) {
                                item.quality = item.quality + 1;
                            }
                        }
                    }
                }
            }

            // ----- Block 2 ------ Item reduces by a day - 1 (unless SHoR)
            if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                item.sellIn = item.sellIn - 1;
            }

            // ----- Block 3 ----- Item sell date is less than 0
            handleIfExpired(item);
        }
    }

    private void handleIfExpired(Item item) {
        if (item.sellIn < 0) {

            handleExpired(item);
        }
    }

    private void handleExpired(Item item) {
        // Reduce quality of items by - 1 if greater than 0 and not AB, BptaTc or SHoR
        if (!item.name.equals("Aged Brie")) {
            if (!item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                if (item.quality > 0) {
                    if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                        item.quality = item.quality - 1;
                    }
                }
            } else {
                // Item quality goes to 0 of BptaTc as item sell date is less than 0
                item.quality = item.quality - item.quality;
            }

        // Increase quality of item if AB
        } else {
            if (item.quality < 50) {
                item.quality = item.quality + 1;
            }
        }
    }
}
