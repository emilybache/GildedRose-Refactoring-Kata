package com.gildedrose.items;

import com.gildedrose.BaseItem;
import com.gildedrose.Item;
import com.gildedrose.ItemInterface;

/**
 * Class for Aged Brie item inherited from NormalItem
 */
public class AgedBrie extends BaseItem implements ItemInterface {

    public AgedBrie(Item item) {
        this.item=item;
    }

    public void updateQuality() {
        if (itemHasExpired()) {
            increaseQualityBy(2);
        } else {
            increaseQualityBy(1);
        }
    }

    public void updateNumberOfdayToSellRemaining() {
        item.sellIn -= 1;
    }
}
