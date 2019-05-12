package com.gildedrose.items;

import com.gildedrose.Item;
import com.gildedrose.BaseItem;
import com.gildedrose.ItemInterface;

/**
 * Class inherited by BaseItem
 */
public class SulfuraItem extends BaseItem implements ItemInterface {
    public SulfuraItem(Item item) {
        this.item=item;
    }

    public void updateQuality() { }

    public void updateNumberOfdayToSellRemaining() { }
}
