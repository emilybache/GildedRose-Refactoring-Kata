package com.gildedrose.utils;

/**
 * <p>Builder for the object {@link EncapsulatedItem}.</p>
 */
public class EncapsulateItemBuilder {

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
     * Constructs a new instance of Encapsulate ItemBuilder which initialize
     * the {@code name}, {@code sellIn} and {@code quality}.
     */
    public EncapsulateItemBuilder() {
        this.name = "";
        this.quality = 0;
        this.sellIn = 0;
    }
    /**
     * <p>Set the name of the item.</p>
     */
    public EncapsulateItemBuilder named(final String name) {
        this.name = name;
        return this;
    }
    /**
     * <p>Set the quality value.</p>
     */
    public EncapsulateItemBuilder ofQuality(final int quality) {
        this.quality = quality;
        return this;
    }
    /**
     * <p>Set the sellIn value.</p>
     */
    public EncapsulateItemBuilder toSellIn(final int sellIn) {
        this.sellIn = sellIn;
        return this;
    }

    /**
     * Create the {@link EncapsulatedItem} object from all the previously set data.
     */
    public EncapsulatedItem build() {
        return new EncapsulatedItem(name, sellIn, quality);
    }
}
