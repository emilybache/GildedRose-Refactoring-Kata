package com.gildedrose;

import com.sun.xml.internal.rngom.parse.host.Base;

/**
 * Class for the item ConjuredItem inherited from NormalItem
 */
public class ConjuredItem extends BaseItem implements ItemInterface {
    public ConjuredItem(Item item) {
        this.item=item;
    }

    public void updateQuality() {
        decreaseQualityBy(1);
        if (itemHasExpired()) {
            decreaseQualityBy(1);
        }
    }
}
