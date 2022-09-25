package com.gildedrose;

class GildedRose {

    public static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    public static final String CHEESE = "Aged Brie";
    //TODO: brainstorm the naming a bit here.
    public static final String BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT
        = "Backstage passes to a TAFKAL80ETC concert";

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    /**
     * Update the staet of the items.
     */
    public void updateQuality() {
        for (Item item : items) {
            if (!item.name.equals(CHEESE)
                && !item.name.equals(BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT)) {
                if (item.quality > 0) {
                    if (!item.name.equals(SULFURAS)) {
                        item.quality = item.quality - 1;
                    }
                }
            } else {
                dealWithPasses(item);
            }

            if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                decrementSellIn(item);
            }

            if (item.sellIn < 0) {
                if (!item.name.equals(CHEESE) &&
                    !item.name.equals(BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT)) {
                    dealWithExpiredItem(item);
                } else {
                    dealWithExpiredCheeseOrPasses(item);
                }
            }
        }
    }

    private void decrementSellIn(Item item) {
        item.sellIn = item.sellIn - 1;
    }

    private void dealWithPasses(Item item) {
        if (item.quality < 50) {
            item.quality = item.quality + 1;

            if (item.name.equals(BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT)) {
                if (item.sellIn < 11) {
                    if (item.quality < 50) {
                        item.quality = item.quality + 1;
                    }
                }

                // If the sellIn is 5 days or fewer add to the quality again
                if (item.sellIn < 6) {
                    item.quality = item.quality + 1;
                }
            }
        }
    }

    private void dealWithExpiredItem(Item item) {
        if (item.quality > 0) {
            if (!item.name.equals(SULFURAS)) {
                item.quality = item.quality - 1;
            }
        } else {
            item.quality = 0;
        }
    }

    private void dealWithExpiredCheeseOrPasses(Item item) {
        if (item.quality < 50 && !item.name.equals(BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT)) {
            item.quality = item.quality + 1;
        } else {
            item.quality = 0;
        }
    }

}
