package com.gildedrose;

import com.gildedrose.utils.EncapsulatedItem;
import com.gildedrose.utils.EncapsulateItemBuilder;

public class BaseTest {

    /**
     * Create a {@link EncapsulatedItem} object given the {@code code}, {@code sellIn} and {@code quality}.
     * @param name The name of the product.
     * @param sellIn  The number of days we have to sell the item
     * @param quality The value which denotes how valuable the item is
     */
    protected EncapsulatedItem createItem(final String name, final int sellIn, final int quality){
        return new EncapsulateItemBuilder()
                .named(name)
                .ofQuality(quality)
                .toSellIn(sellIn)
                .build();
    }
    /**
     * Create the instance of {@link GildedRose} and update the Quality of the {@code items}
     */
    protected void updateQuality(final EncapsulatedItem...items){
        new GildedRose(items).updateQuality();
    }

}
