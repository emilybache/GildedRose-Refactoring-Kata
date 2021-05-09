package com.gildedrose.updater;

import com.gildedrose.product.category.Conjured;
import com.gildedrose.utils.Articles;
import com.gildedrose.product.category.AgedBrie;
import com.gildedrose.product.category.BackStagePass;
import com.gildedrose.product.category.Legendary;
import com.gildedrose.utils.EncapsulatedItem;

import java.util.Objects;

/**
 * <p>Factory class that is responsible for returning the right object type from a given item.</p>
 */
public class UpdaterFactory {

    /**
     * Main method of the factory. It receives an item and based on the name it
     * generates an object according to the category.</p>
     *
     *  <p>If the name of the item can't found in the {@link Articles} will be return a default implementation
     *  {@link GenericUpdater} </p>
     *
     * These can be:
     * <ul>
     *     <li>AgedBrie {@link AgedBrie}</li>
     *     <li>BackStagePass {@link BackStagePass}</li>
     *     <li>Conjured {@link Conjured}</li>
     *      <li>Legendary {@link Legendary}</li>
     * </ul>
     * @see AgedBrie
     * @see Legendary
     * @see BackStagePass
     * @see Conjured
     * @see GenericUpdater
     * @return  a new implementation of {@link Updater} given the input {@code item},
     * @throws NullPointerException if {@code item} is {@code null}
     */
    public static Updater getUpdater(final EncapsulatedItem item) {
        Objects.requireNonNull(item, "Null EncapsulateItem items");
        switch (Articles.getArticle(item.getName())){
            case AGED_BRIE:
               return new AgedBrie<>(item);
            case SULFURAS:
                return new Legendary<>(item);
            case TAFKAL_80ETC:
                return new BackStagePass<>(item);
            case CONJURED:
                return new Conjured<>(item);
            default:
              return new GenericUpdater<>(item);
        }
    }
}
