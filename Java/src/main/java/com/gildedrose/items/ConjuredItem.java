package com.gildedrose.items;

import com.gildedrose.BaseItem;
import com.gildedrose.Item;
import com.gildedrose.ItemInterface;

/**
 * Class for the item ConjuredItem inherited from BaseItem
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

    public void updateNumberOfdayToSellRemaining() {
        item.sellIn -= 1;
    }
}
