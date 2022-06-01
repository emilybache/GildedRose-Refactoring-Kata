/* (C)2022 */
package com.gildedrose.item;

public class Conjured extends Item {

    public static final String CONJURED = "Conjured";

    public Conjured(int sellIn, int quality) {
        super(CONJURED, sellIn, quality);
    }

    @Override
    public void updateQuality() {
        if (sellIn <= 0) {
            quality -= 4;
        } else {
            quality -= 2;
        }
        if (quality < 0) {
            quality = 0;
        }
    }
}
