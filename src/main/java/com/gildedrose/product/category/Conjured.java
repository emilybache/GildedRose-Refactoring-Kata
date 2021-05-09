package com.gildedrose.product.category;

import com.gildedrose.updater.GenericUpdater;
import com.gildedrose.utils.EncapsulatedItem;

/**
 * Conjures class handler ().
 * Conditions:
 * <ul>
 *     <li>"Conjured" items degrade in Quality twice as fast as normal items.</li>
 * </ul>
 *
 */
public class Conjured <T extends EncapsulatedItem> extends GenericUpdater<T> {

    private static final int decrementFactor = 2;
    private static final int incrementFactor = 2;

    /**
     * Constructor that creates a Conjured Object.
     * @param item  the Conjured {@link EncapsulatedItem}
     */
    public Conjured(final T item) {
        super(item, incrementFactor, decrementFactor);
    }
    /**
     * {@inheritDoc}
     */
    protected void updateQuality(final EncapsulatedItem item) {
        lowerQuality(item);
    }
}
