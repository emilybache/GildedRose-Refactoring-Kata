package com.gildedrose.model;

import static com.gildedrose.Constants.MAX_QUALITY;
import static com.gildedrose.Constants.MIN_QUALITY;

public class BackstagePasses extends Goods{

    public BackstagePasses( int sellIn, int quality) {
        super("Backstage passes to a TAFKAL80ETC concert", sellIn, quality);
    }

    @Override
    public void updateQuality() {

        if(isSellInLessThanOrEquals(0)) {
                degradeQualityToZero();
        }
        else if(isSellInLessThanOrEquals(5)) {
            upgradeQuality(3);
        }
        else if (isSellInLessThanOrEquals(10)) {
            upgradeQuality(2);
        } else {
            upgradeQuality(1);
        }
        sellInPasses();
    }

    private boolean isSellInLessThanOrEquals(int numberOfDays) {
        return sellIn <= numberOfDays;
    }

    private void upgradeQuality(int byValue) {
        quality =  Math.min(quality + byValue, MAX_QUALITY);
    }

    private void degradeQualityToZero() {
        quality = MIN_QUALITY;
    }
}
