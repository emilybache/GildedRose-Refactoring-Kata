package com.gildedrose.product.category;

import com.gildedrose.Item;
import com.gildedrose.updater.GenericUpdater;
import com.gildedrose.utils.EncapsulatedItem;

/**
 * Aged Brie class handler.
 * Conditions:
 * <ul>
 *     <li>"Aged Brie" actually increases in Quality the older it gets</li>
 *     <li>Increases Expired Date</li>
 * </ul>
 *
 */
public class AgedBrie<T extends EncapsulatedItem> extends GenericUpdater<T> {


    /**
     * Constructor that creates a AgedBrie Object.
     * @param item  the Age Brie {@link Item}
     */
    public AgedBrie(final T item) {
        super(item);
    }
    /**
     * "Aged Brie" actually increases in Quality the older it gets
     */
    protected void updateAfterSaleDate(final EncapsulatedItem item) {
        increasesQuality(item);
    }
    /**
     * {@inheritDoc}
     */
    protected void updateQuality(final EncapsulatedItem item) {
        increasesQuality(item);
    }
}
