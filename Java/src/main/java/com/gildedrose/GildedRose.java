package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            String name = item.name;
            updateSellIn(item, name);
            switch (name) {
                case "Sulfuras, Hand of Ragnaros":
                    break;
                case "Aged Brie":
                    increaseQualityByOne(item);
                    break;
                case "Backstage passes to a TAFKAL80ETC concert":
                    updateBackstagePassesQuality(item);
                    break;
                case "Conjured Mana Cake":
                    updateConjuredQuality(item);
                    break;
                default:
                    updateQualityOfDefaultItems(item);
                    break;
            }
        }
    }

    private void updateQualityOfDefaultItems(Item item) {
        if (item.sellIn >= 0)
            decreaseQualityByOne(item);
        else {
            decreaseQualityByOne(item);
            decreaseQualityByOne(item);
        }
    }

    private void decreaseQualityByOne(Item item) {
        if (item.quality > 0)
            item.quality--;
    }

    private void updateSellIn(Item item, String name) {
        if (!name.equals("Sulfuras, Hand of Ragnaros")) {
            item.sellIn = item.sellIn - 1;
        }
    }

    private void updateConjuredQuality(Item item) {
        decreaseQualityByOne(item);
        decreaseQualityByOne(item);
    }

    private void increaseQualityByOne(Item item) {
        if (isValueLessThanBy(item.quality, 50)) {
            item.quality++;
        }
    }

    private boolean isValueLessThanBy(int value, int target) {
        return value < target;
    }

    /*Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
    Quality drops to 0 after the concert*/
    private void updateBackstagePassesQuality(Item item) {
        if (isValueLessThanBy(item.sellIn, 11)) {
            increaseQualityByOne(item);
            increaseQualityByOne(item);
        }
        if (isValueLessThanBy(item.sellIn, 6) || item.sellIn > 10)
            increaseQualityByOne(item);
    }
}
