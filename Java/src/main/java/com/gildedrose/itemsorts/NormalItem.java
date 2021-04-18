package com.gildedrose.itemsorts;

import com.gildedrose.itemsorts.QualityItem.QualityItemSkeleton;

public class NormalItem extends QualityItemSkeleton implements QualityItem {
    public NormalItem(String name, int sellIn, int quality) {
        super(name, sellIn, quality);
    }
}
