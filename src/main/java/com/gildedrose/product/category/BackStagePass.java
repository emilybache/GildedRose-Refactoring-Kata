package com.gildedrose.product.category;

import com.gildedrose.updater.GenericUpdater;
import com.gildedrose.utils.EncapsulatedItem;

/**
 * BackstagePass class handler ().
 * Conditions:
 * <ul>
 *     <li>Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less.</li>
 *     <li>Quality drops to 0 after the concert</li>
 * </ul>
 *
 */
public class BackStagePass<T extends EncapsulatedItem> extends GenericUpdater<T> {

    /**
     * Represent 5 days
     */
    private static final int QUALITY_DAY_X2 = 5;
    /**
     * Represent 10 days
     */
    private static final int QUALITY_DAY_X3 = 10;

    /**
     * Constructor that creates a BackstagePass Object.
     * @param item  the Back Stage Pass {@link EncapsulatedItem}
     */
    public BackStagePass(final T item) {
        super(item);
    }
    /**
     * <p>Quality drops to 0 after the concert.</p>
     */
    protected void updateAfterSaleDate(final EncapsulatedItem item) {
        item.setQuality(0);
    }
    /**
     * <p>Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less.</p>
     */
    protected void updateQuality(final EncapsulatedItem item) {
        increasesQuality(item);
        if (item.getSellIn() <= QUALITY_DAY_X3) {
            increasesQuality(item);
        }
        if (item.getSellIn() <= QUALITY_DAY_X2) {
            increasesQuality(item);
        }
    }
}
