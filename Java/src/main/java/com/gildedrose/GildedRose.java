package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    private void increaseQuality(Item item) {
        if (item.quality < 50) {
            item.quality += 1;
        }
    }

    private void decreaseQuality(Item item) {
        if (item.quality > 0) {
            item.quality -= 1;
        }
    }

    private void updateNumberOfdayToSellRemaining(Item item) {
        item.sellIn -= 1;
    }

    private boolean itemHasExpired(Item item) {
        boolean condition;
        if (item.sellIn < 0) {
            condition=true;
        } else {
            condition=false;
        }
        return condition;
    }

    public void updateQuality() {
        String SULFURA = "Sulfuras, Hand of Ragnaros";
        String AGED_BRIE = "Aged Brie";
        String BACKSTAGE = "Backstage passes to a TAFKAL80ETC concert";


        for (Item item : items) {
            if (item.name.equals(SULFURA)) {
                continue;
            }

            updateNumberOfdayToSellRemaining(item);

            if (item.name.equals(AGED_BRIE)) {
                increaseQuality(item);
            } else if (item.name.equals(BACKSTAGE)) {
                increaseQuality(item);

                if (item.sellIn < 10) {
                    increaseQuality(item);
                }
                if (item.sellIn < 5) {
                    increaseQuality(item);
                }

            } else {
                decreaseQuality(item);
            }


            if (itemHasExpired(item)) {
                if (item.name.equals(AGED_BRIE)) {
                    increaseQuality(item);
                } else if (item.name.equals(BACKSTAGE)) {
                    item.quality -= item.quality;
                } else {
                    decreaseQuality(item);
                }

            }
        }
    }
}
