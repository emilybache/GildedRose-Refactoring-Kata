package com.gildedrose.product.category;

import com.gildedrose.updater.GenericUpdater;
import com.gildedrose.utils.EncapsulatedItem;

/**
 * Legendary class handler ().
 * Conditions:
 * <ul>
 *     <li>Never has to be sold or decreases in Quality.</li>
 * </ul>
 *
 */
public class Legendary <T extends EncapsulatedItem> extends GenericUpdater<T> {

    //TODO: All the legendary item have immutable quality of 80 or just only the Sulfuras?
    /**
     * Constructor that creates a Legendary Object.
     * @param item  the Legendary {@link EncapsulatedItem}
     */
    public Legendary(final T item) {
        super(item);
    }
    /**
     * {@inheritDoc}
     */
    protected void updateAfterSaleDate(final EncapsulatedItem item) {
        // do nothing
    }
    /**
     * {@inheritDoc}
     */
    protected void updateSellIn(final EncapsulatedItem item) {
        // do nothing
    }
    /**
     * {@inheritDoc}
     */
    protected void updateQuality(final EncapsulatedItem item) {
        // do nothing
    }
}
