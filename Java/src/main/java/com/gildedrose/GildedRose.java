package com.gildedrose;

class GildedRose {

    private static final String BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT = "Backstage passes to a TAFKAL80ETC concert";
    private static final String AGED_BRIE = "Aged Brie";
    private static final String SULFURAS_HAND_OF_RAGNAROS = "Sulfuras, Hand of Ragnaros";
    private static final String CONJURED_MANA_CAKE = "Conjured Mana Cake";
    private static final int MAX_QUALITY = 50;

    Item[] items;

    /**
     * Creates a new GildedRose {@link GildedRose}
     * @param items The items of the {@link GildedRose} rose
     */
    public GildedRose(Item[] items) {
        this.items = items;
    }

    /**
     * Updates the quality of every item in the list
     */
    public void updateQuality() {
        for (Item item : items) {
            switch (item.name) {
                case BACKSTAGE_PASSES_TO_A_TAFKAL_80_ETC_CONCERT:
                    processBackstage(item);
                    break;
                case AGED_BRIE:
                    processAgeBrie(item);
                    break;
                case SULFURAS_HAND_OF_RAGNAROS:
                    continue;
                case CONJURED_MANA_CAKE:
                    processConjured(item);
                    break;
                default:
                    processGeneral(item);
            }
            commonEndProcess(item);
        }
    }

    /**
     * Decreases the quality of a general item
     * 1 decrease within the exiry date: item.sellIn >0
     * 2 decrease after it expires: item.sellIn <= 0
     * Never decreases after 0. Quality never below 0
     *
     * @param item Item {@link Item}
     */
    private void processGeneral(Item item) {
        decreaseQualityConditionally1X(item);
    }

    /**
     * Decreases the quality of a conjured item
     * 2 decrease within the exiry date: item.sellIn >0
     * 4 decrease after it expires: item.sellIn <= 0
     * Never decreases after 0. Quality never below 0
     *
     * @param item Item {@link Item}
     */
    private void processConjured(Item item) {
        decreaseQualityConditionally2X(item);
    }

    /**
     * Increases the quality of a Age Brie item
     * 1 increase within the exiry date: item.sellIn >0
     * 2 increase after it expires: item.sellIn <= 0
     * Never increases after 50. Quality never over 50
     *
     * @param item Item {@link Item}
     */
    private void processAgeBrie(Item item) {
        if (item.sellIn > 0) {
            increaseQualityConditionally1X(item);
        } else {
            increaseQualityConditionally2X(item);
        }
    }

    /**
     * Increases the quality of a Backstage Ticket item
     * 1 increase if there are still more than 10 days to go
     * 2 increase if there are still between 10 days (inclusive) and 5 (exclusive) to go
     * 3 increase if there are still between 5 days (inclusive) and 0 (exclusive) to go
     * Quality is set to 0 once the concert is done and the ticket has expired
     * Quality never below zero.
     *
     * @param item Item {@link Item}
     */
    private void processBackstage(Item item) {
        if (item.sellIn > 10) {
            increaseQualityConditionally1X(item);
        } else if (item.sellIn > 5) {
            increaseQualityConditionally2X(item);
        } else if (item.sellIn > 0) {
            increaseQuality3X(item);
        } else {
            item.quality = 0;
        }
    }

    /**
     * Auxiliary method to perform the conditional quality increase by one fold.
     * It calls a common method which receives 1 as a fold parameter.
     *
     * @param item Item {@link Item}
     */
    private void increaseQualityConditionally1X(Item item) {
        item.quality = getQualityIncrease(item.quality, 1);
    }

    /**
     * Auxiliary method to perform the conditional quality increase by two fold.
     * It calls a common method which receives 2 as a fold parameter.
     *
     * @param item Item {@link Item}
     */
    private void increaseQualityConditionally2X(Item item) {
        item.quality = getQualityIncrease(item.quality, 2);
    }

    /**
     * Auxiliary method to perform the conditional quality increase by three fold.
     * It calls a common method which receives 3 as a fold parameter.
     *
     * @param item Item {@link Item}
     */
    private void increaseQuality3X(Item item) {
        item.quality = getQualityIncrease(item.quality, 3);
    }

    /**
     * Auxiliary method to perform the conditional quality decrease by one fold.
     * It calls a common method which receives 1 as a fold parameter.
     *
     * @param item Item {@link Item}
     */
    private void decreaseQualityConditionally1X(Item item) {
        decreaseQualityByFold(item, 1);
    }

    /**
     * Auxiliary method to perform the conditional quality decrease by two fold.
     * It calls a common method which receives 2 as a fold parameter.
     *
     * @param item Item {@link Item}
     */
    private void decreaseQualityConditionally2X(Item item) {
        decreaseQualityByFold(item, 2);
    }

    /**
     * Support method for conditional quality decrease
     * It performs a conditional decrease by the fold.
     * Within the expiry date means an attempt to decrease 1 * fold: sellIn > 0 => 1 * fold
     * Outside the expiry date means an attempt to decrease 2 * fold: sellIn <=0 => 2 * fold
     *
     * @param item Item {@link Item}
     * @param fold Fold numbrt
     */
    private void decreaseQualityByFold(Item item, int fold) {
        if (item.sellIn > 0) {
            item.quality = decreaseQualityConditionally(item.quality, fold);
        } else {
            item.quality = decreaseQualityConditionally(item.quality, fold * 2);
        }
    }

    /**
     * Calculates the new quality value in a conditional way
     * The returned value is never zero.
     *
     * @param quality   The value of the quality
     * @param decrement The decrement value
     * @return New quality value
     */
    private int decreaseQualityConditionally(int quality, int decrement) {
        if (quality >= decrement) {
            return quality - decrement;
        }
        return 0;
    }

    /**
     * Calculates the new quality value in a conditional way.
     * The returned value is never greater than the maximum quality.
     *
     * @param quality   The value of the quality
     * @param increment The decrement value
     * @return New quality value
     */
    private int getQualityIncrease(int quality, int increment) {
        if (quality <= MAX_QUALITY - increment) {
            return quality + increment;
        }
        return MAX_QUALITY;
    }

    /**
     * Decreases the sellIn.
     *
     * @param item Item {@link Item}
     */
    private void commonEndProcess(Item item) {
        item.sellIn--;
    }
}