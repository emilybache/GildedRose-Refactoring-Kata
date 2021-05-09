package com.gildedrose.updater;

import com.gildedrose.Item;
import com.gildedrose.product.category.AgedBrie;
import com.gildedrose.product.category.BackStagePass;
import com.gildedrose.product.category.Conjured;
import com.gildedrose.product.category.Legendary;
import com.gildedrose.utils.EncapsulatedItem;

/**
 * Generic updater for products. All those products that do not require a custom
 * update can / should use this class and the default methods.
 */
public class GenericUpdater<T extends EncapsulatedItem> implements Updater  {

    /**
     * <p>the value associated with the instance to decrement.</p>
     */
    private final int decrementQualityFactor;
    /**
     * <p>the value associated with the instance to increment.</p>
     */
    private final int incrementQualityFactor;
    /**
     * <p>The maximum size to which the quality can be.</p>
     */
    private static final int MAX_QUALITY = 50;

    /**
     * <p>The {@link EncapsulatedItem} to update.</p>
     */
    private final EncapsulatedItem item;

    /**
     * <p>Constructor for GenericUpdater.</p>
     *
     * @param item the {@link EncapsulatedItem} to manipulate
     */
    public GenericUpdater(final T item) {
        this.item = item;
        this.incrementQualityFactor = 1;
        this.decrementQualityFactor = 1;
    }

    /**
     * <p>Constructor for GenericUpdater.</p>
     *
     * @param item the {@link Item} to update
     * @param incrementQualityFactor the value associated with the instance to increment
     * @param decrementQualityFactor  the value associated with the instance to decrement
     */
    public GenericUpdater(final T item, final int incrementQualityFactor, final int decrementQualityFactor) {
        this.item = item;
        this.incrementQualityFactor = incrementQualityFactor;
        this.decrementQualityFactor = decrementQualityFactor;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update() {
        updateQuality(item);
        updateSellIn(item);
        if (item.getSellIn() < 0) {
            updateAfterSaleDate(item);
        }
    }

    /**
     * <p>Increment the quality of tge given {@link EncapsulatedItem}.</p>
     *
     * @param item the {@link EncapsulatedItem} to update the quality.
     *
     */
        protected void increasesQuality(final EncapsulatedItem item) {
        if (item.getQuality() < MAX_QUALITY) {
            item.setQuality(item.getQuality() + incrementQualityFactor);
        }
    }
    /**
     * <p>Update the expired properties of tge given {@link EncapsulatedItem}.</p>
     * <p>It depends on each product how to update the quality after the sale date</p>
     *
     * @see AgedBrie
     * @see Legendary
     * @see BackStagePass
     * @see Conjured
     *
     * @param item the {@link EncapsulatedItem} to update the quality.
     */
    protected void updateAfterSaleDate(final EncapsulatedItem item) {
        lowerQuality(item);
    }
    /**
     * <p>Update the Quality properties of tge given {@link EncapsulatedItem}.</p>
     *
     * @param item the {@link EncapsulatedItem} to update the quality.
     *
     */
    protected void updateQuality(final EncapsulatedItem item) {
        lowerQuality(item);
    }
    /**
     * <p>Decrement the quality of tge given {@link EncapsulatedItem}.</p>
     *
     * @param item the {@link EncapsulatedItem} to update the quality.
     *
     */
    protected void lowerQuality(final EncapsulatedItem item) {
        if (item.getQuality() > 0) {
            item.setQuality(item.getQuality() - decrementQualityFactor);
        }
    }
    /**
     * <p>Update the sell in properties of tge given {@link EncapsulatedItem}.</p>
     *
     * @param item the {@link EncapsulatedItem} to update the quality.
     *
     */
    protected void updateSellIn(final EncapsulatedItem item) {
        item.setSellIn(item.getSellIn() - 1);
    }
}
