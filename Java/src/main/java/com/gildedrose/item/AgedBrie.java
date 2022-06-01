/* (C)2022 */
package com.gildedrose.item;

public class AgedBrie extends Item {

    public static final String AGED_BRIE = "Aged Brie";

    public AgedBrie(int sellIn, int quality) {
        super(AGED_BRIE, sellIn, quality);
    }

    @Override
    public void updateQuality() {
        if (quality < 50) {
            if (sellIn <= 0) {
                quality += 2;
            } else {
                quality += 1;
            }
        }
    }
}
