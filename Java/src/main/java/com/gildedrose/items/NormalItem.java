package com.gildedrose.items;

import com.gildedrose.BaseItem;
import com.gildedrose.Item;
import com.gildedrose.ItemInterface;

/**
 * Class for a regular item with business rule for normal item:
 *  If the item expired => reduce the quality by 2 else reduce by 1
 *  Quality for an item is never negative
 *  Quality for an item is not greater than the constant MAX_QUAILITY_FOR_AN_ITEM
 */
public class NormalItem extends BaseItem implements ItemInterface {
    public NormalItem(Item item) {
        this.item = item;
    }

    public void updateQuality() {
        if (itemHasExpired()) {
            decreaseQualityBy(2);
        } else {
            decreaseQualityBy(1);
        }
    }

    public void updateNumberOfdayToSellRemaining() {
        item.sellIn -= 1;
    }
}
