package com.gildedrose.updater;

import com.gildedrose.utils.EncapsulatedItem;

/**
 * Base interface from where all the different product updaters will implement.
 * Contain only one abstract method {@code update}
 */
public interface Updater{

    /**
     * Updates the {@link EncapsulatedItem} properties
     */
    void update();
}
