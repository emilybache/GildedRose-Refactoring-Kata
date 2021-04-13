package com.gildedrose.behavior.quality;

import com.gildedrose.Item;

public class DefaultQualityBehavior implements QualityBehavior {

    public static final int MAX_QUALITY_LEVEL = 50;
    public static final int MIN_QUALITY_LEVEL = 0;
    public static final int DEFAULT_QUALITY_DECREASE = 1;
    public static final int FASTER_QUALITY_DECREASE = 2;

    public static DefaultQualityBehavior newInstance() {
        return new DefaultQualityBehavior();
    }

    @Override
    public void processQualityUpdate(Item item) {
        decreaseQuality(item);
    }

    private void decreaseQuality(Item item) {
        item.quality = limitQuality(item.quality - getQualityDecreaseAmount(item));
    }

    private int getQualityDecreaseAmount(Item item) {
        return item.sellIn > 0 ? DEFAULT_QUALITY_DECREASE : FASTER_QUALITY_DECREASE;
    }
    private int limitQuality(int newQuality) {
        return Math.max(MIN_QUALITY_LEVEL, Math.min(MAX_QUALITY_LEVEL, newQuality));
    }
}
