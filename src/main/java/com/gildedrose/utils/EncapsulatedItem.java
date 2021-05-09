package com.gildedrose.utils;

import java.io.Serializable;

/**
 * <p>Item copy object with all data encapsulated.</p>
 */
public class EncapsulatedItem implements Serializable {

    private static final long serialVersionUID = 1989175797960240011L;
    /**
     * The name of the product.
     */
    private String name;
    /**
     * SellIn value which denotes the number of days we have to sell the item.
     */
    private int sellIn;
    /**
     * Quality value which denotes how valuable the item is.
     */
    private int quality;

    /**
     * Constructs a new instance of EncapsulateItem with the given {@code name}, {@code sellIn} and {@code quality}.
     * @param name The name of the product.
     * @param sellIn  The number of days we have to sell the item
     * @param quality The value which denotes how valuable the item is
     */
    public EncapsulatedItem(final String name, final int sellIn, final int quality) {
        this.setName(name);
        this.setSellIn(sellIn);
        this.setQuality(quality);
    }

    /**
     * <p>Gets the name suitable for display.</p>
     *
     * @return the name.
     */
    public String getName() {
        return name;
    }

    /**
     * <p>Set the name of the item.</p>
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * <p>Gets the sell in suitable for display.</p>
     *
     * @return the sellIn value.
     */
    public int getSellIn() {
        return sellIn;
    }
    /**
     * <p>Set the sellIn value.</p>
     */
    public void setSellIn(final  int sellIn) {
        this.sellIn = sellIn;
    }
    /**
     * <p>Gets the quality suitable for display.</p>
     *
     * @return the quality value.
     */
    public int getQuality() {
        return quality;
    }
    /**
     * <p>Set the quality value.</p>
     */
    public void setQuality(final int quality) {
        this.quality = quality;
    }

    @Override
    public String toString() {
        return this.getName() + ", " + this.getSellIn() + ", " + this.getQuality();
    }
}
