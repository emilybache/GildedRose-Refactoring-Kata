package com.gildedrose.behavior.quality;

import com.gildedrose.Item;

public class ImmutableQualityBehavior implements QualityBehavior {

    public static ImmutableQualityBehavior newInstance() {
        return new ImmutableQualityBehavior();
    }

    @Override
    public void processQualityUpdate(Item item) {

    }

}
