/* (C)2022 */
package com.gildedrose.item;

public class BackstageTicket extends Item {
    public static final String BACKSTAGE_TICKET = "Backstage passes to a TAFKAL80ETC concert";

    public BackstageTicket(int sellIn, int quality) {
        super(BACKSTAGE_TICKET, sellIn, quality);
    }

    @Override
    public void updateQuality() {
        if (quality < 50) {
            if (sellIn <= 0) {
                quality = 0;
            } else if (sellIn < 6) {
                quality += 3;
            } else if (sellIn < 11) {
                quality += 2;
            } else {
                quality += 1;
            }
        }
    }
}
